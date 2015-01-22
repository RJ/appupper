%% rewrite app and app.src files to update vsn field
-module(appupper_versionbumper).
-compile(export_all).

-define(AppHeader, "%% Vsn auto-managed by appupper utility.\n%% DO NOT CHANGE VSN FIELD MANUALLY!").
-define(is_level(Level), (Level == major orelse Level == minor orelse Level == patch)).

int(Str) ->
    {I,""} = string:to_integer(Str), I.

str2vsn(Str) ->
    case string:tokens(Str, ".") of
        [Maj,Min,Pat] ->
            {int(Maj), int(Min), int(Pat)};
        [Maj,Min] ->
            {int(Maj), int(Min), 0};
        [Maj] ->
            {int(Maj), 0, 0}
    end.

vsn2str({Maj,Min,Pat}) ->
    lists:flatten(io_lib:format("~B.~B.~B",[Maj,Min,Pat])).

bump_vsn({Maj,Min,Pat}, major) -> {Maj+1, Min, Pat};
bump_vsn({Maj,Min,Pat}, minor) -> {Maj, Min+1, Pat};
bump_vsn({Maj,Min,Pat}, patch) -> {Maj, Min, Pat+1}.

%%  bump_version("1.0.0", patch) --> "1.0.1"
bump_version(Str, Level) when is_list(Str) andalso ?is_level(Level) ->
    vsn2str(bump_vsn(str2vsn(Str), Level)).

%app_src_path_to_app_path(Path) ->
    %%% TODO oh yeah, there's a "filename" module..
    %Parts = string:tokens(Path, "/"),
    %NewParts = lists:sublist(Parts, length(Parts)-2) ++ ["ebin", tl(Parts)],
    %string:join(NewParts, "/").

rewrite_appfile_inplace(Filepath, BumperFun) when is_list(Filepath), is_function(BumperFun,1) ->
    io:format("rewriting inplace ~s\n",[Filepath]),
    {ok, [{application, AppName, Sections}]} = file:consult(Filepath),
    Vsn = proplists:get_value(vsn, Sections),
    NewVsn = BumperFun(Vsn),
    NewSections = [{vsn, NewVsn} | proplists:delete(vsn, Sections)],
    Contents = io_lib:format("~s\n~p.~n",[?AppHeader, {application, AppName, NewSections}]),
    ok = file:write_file(Filepath, Contents),
    io:format("Rewrote ~s, changing version from: ~s to: ~s\n",[Filepath, Vsn, NewVsn]),
    {ok, Vsn, NewVsn}.

%% takes path to .app.src, bumps that and the compiled .app file in ../ebin
%% this avoids having to run make just to rewrite the .app file vsn
bump_dot_apps(AppFile, AppSrcFile, Level) ->
    %% bump the .app.src, then use the new vsn to replace the .app file too
    Bumper = fun(V) -> bump_version(V,Level) end,
    case rewrite_appfile_inplace(AppSrcFile, Bumper) of
        {ok, OldVsn, NewVsn} ->
            FixedBumper = fun(_OldVsn) -> NewVsn end,
            {ok, OldVsn, NewVsn} = rewrite_appfile_inplace(AppFile, FixedBumper),
            {ok, OldVsn, NewVsn}
    end.

bump_relx_vsn(File, RelName, PrevVsn) when is_atom(RelName), is_list(PrevVsn) ->
    {ok, Terms} = file:consult(File),
    PrevRelease = case lists:filter(
            fun
                (T) when is_tuple(T) ->
                    element(1, T) == release andalso
                    element(2, T) == {RelName, PrevVsn};
                (_) ->
                    false
            end, Terms) of
        [PR] ->
            PR;
        [] ->
            throw(no_previous_release_term_in_relx_config)
    end,
    NewVsn = bump_version(PrevVsn, minor),
    NewReleaseTuple = setelement(2, PrevRelease, {RelName, NewVsn}),
    io:format("Updating relx.config, added new release @ ver ~s\n",[NewVsn]),
    ok = file:write_file(File, io_lib:format("\n\n~p.",[NewReleaseTuple]), [append]),
    {ok, NewVsn}.
    
