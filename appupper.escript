#!/usr/bin/env escript
%%! -noshell -noinput
%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=4 sw=4 et
-record(app, {name, vsn, path, ebin, mods=[]}).
-record(release, {name, vsn, apps=[]}).

%% Design notes:
%% 1) Construct two #releases{} containing a list of #app{} each
%% 2) Compare apps from the two releases, and generate .appups
%% 3) Profit.

main([]) ->
    Apps = parse_local_appvers(["apps"]),
    print_apps_summary(Apps),
    ok;

%% working copy vs last _rel version
main([RelName, RelVsn]) ->
    OldRelPath = filename:join(["_rel","releases",RelVsn, RelName ++ ".rel"]),
    FilterFun = make_appname_filter(),
    {RelName, OldVsn, OldApps} = parse_rel_file(OldRelPath, "_rel/lib", FilterFun),
    OldRel = #release{name=RelName, vsn=OldVsn, apps=OldApps},
    NewApps = parse_local_appvers(["apps"]),
    NewRel = OldRel#release{vsn="new", apps=NewApps},
    compare_releases(OldRel, NewRel),
    ok;

main([RelName, OldV, NewV]) ->
    FilterFun = make_appname_filter(),
    OldRelPath = filename:join(["_rel","releases",OldV, RelName ++ ".rel"]),
    NewRelPath = filename:join(["_rel","releases",NewV, RelName ++ ".rel"]),
    {RelName, OldVsn, OldApps} = parse_rel_file(OldRelPath, "_rel/lib", FilterFun),
    {RelName, NewVsn, NewApps} = parse_rel_file(NewRelPath, "_rel/lib", FilterFun),
    OldRel = #release{name=RelName, vsn=OldVsn, apps=OldApps},
    NewRel = #release{name=RelName, vsn=NewVsn, apps=NewApps},
    compare_releases(OldRel, NewRel),
    ok;

main(_) ->
    io:format("Usage: not like that.\n").

%% 
make_appname_filter() ->
    %% only consider app names ^irc.* for testing
    fun(N) ->
        case N of
            "irc"++_ -> true;
            _        -> false
        end
    end.

compare_releases(OldRel = #release{name=RelName,vsn=OV}, 
                 NewRel = #release{vsn=NV}) ->
    print_releases([OldRel, NewRel]),
    io:format("Comparing releases ~s: ~s  -->  ~s~n",[RelName, OV, NV]),
    OldApps = [ {App#app.name, App} || App <- OldRel#release.apps ],
    Apps = lists:filter(fun(A) ->
        proplists:get_value(A#app.name, OldApps) =/= undefined
    end, NewRel#release.apps),
    AppUps = lists:map(fun(App) ->
        OldApp = proplists:get_value(App#app.name, OldApps),
        AppUpPath = filename:join([App#app.path, "ebin", 
                                   atom_to_list(App#app.name) ++ ".appup"]),
        {AppUpPath, make_appup(OldApp, App)}
    end, Apps),
    io:format("~p~n",[AppUps]).

parse_local_appvers(Dirs) ->
    AppFiles = find_app_files(Dirs),
    AppSpecs = lists:map(fun(AppFile) ->
        parse_app_file(AppFile)
    end, AppFiles),
    sort_apps(AppSpecs).


find_app_files(Dirs) -> find_app_files(Dirs, []).

find_app_files([],         Acc) -> Acc;
find_app_files([Dir|Dirs], Acc) ->
    AppFiles = filelib:fold_files(Dir, ".*\\.app$", true, 
                                  fun(F,A) -> [F|A] end, []),
    find_app_files(Dirs, Acc ++ AppFiles).

make_appup(OldApp = #app{}, NewApp = #app{}) ->
    UpInstructions   = make_appup_instructions(OldApp, NewApp),
    DownInstructions = reverse_appup_instructions(UpInstructions),
    {NewApp#app.vsn,
        [{OldApp#app.vsn, UpInstructions}],
        [{OldApp#app.vsn, DownInstructions}]
    }.

make_appup_instructions(OldApp = #app{}, NewApp = #app{}) ->
    OldMods = OldApp#app.mods,
    NewMods = NewApp#app.mods,
    %% Detect newly added, removed, or modules that exist in both apps:
    %% Modules in both old and new will be compared to see if they need loading
    AddedMods   = lists:dropwhile(fun(E)->lists:member(E,OldMods)end, NewMods),
    RemovedMods = lists:dropwhile(fun(E)->lists:member(E,NewMods)end, OldMods),
    SameMods    = sets:to_list(sets:intersection(sets:from_list(OldMods),
                                                 sets:from_list(NewMods))),
    lists:flatten([
        [ {add_module, M} || M <- AddedMods ],
        [ appup_instruction_for_module(M, OldApp, NewApp) || M <- SameMods ],
        [ {delete_module, M} || M <- RemovedMods ]
    ]).

reverse_appup_instructions(L) ->
    reverse_appup_instructions(L,[]).

reverse_appup_instructions([], Acc) -> Acc;
reverse_appup_instructions([I|Rest], Acc) ->
    reverse_appup_instructions(Rest, [reverse_appup_instruction(I) | Acc]).

reverse_appup_instruction({add_module, M})    -> {delete_module, M};
reverse_appup_instruction({delete_module, M}) -> {add_module, M};
reverse_appup_instruction({update, M, {advanced, [A,B]}}) -> {update, M, {advanced, [B,A]}};
reverse_appup_instruction({apply, {M, sup_upgrade_notify, [A,B]}}) -> {apply, {M, sup_upgrade_notify, [B,A]}};
reverse_appup_instruction(Other) -> Other.

appup_instruction_for_module(M, OldApp, NewApp) ->
    OldBeamPath = OldApp#app.path ++ "/ebin/" ++ atom_to_list(M) ++ ".beam",
    NewBeamPath = NewApp#app.path ++ "/ebin/" ++ atom_to_list(M) ++ ".beam",
    {ok, OldBeam} = file:read_file(OldBeamPath),
    {ok, NewBeam} = file:read_file(NewBeamPath),
    %% Is the new module different to the old module?
    case beam_lib:cmp(OldBeam, NewBeam) of
        ok ->
            [];
        {error, beam_lib, ErrRsn} ->
            io:format("beam_lib:cmp(~p) ~p~n",[M, ErrRsn]),
            {module, M} = code:load_binary(M, "/tmp/undefined.beam", NewBeam),
            NewInfo = parse_module_info(M:module_info()),
            HasBehaviour = fun(B) ->
                lists:member(B, proplists:get_value(behaviours, NewInfo, []))
            end,
            Instructions = case HasBehaviour(supervisor) of
                true  -> appup_for_supervisor(M, OldApp, NewApp);
                false ->
                    case module_has_code_change(M) of
                        true  -> appup_for_code_change(M, OldApp, NewApp);
                        false -> [{load_module, M}]
                    end
            end,
            code:delete(M),
            code:purge(M),
            Instructions
    end.

module_has_code_change(M) ->
    erlang:function_exported(M, code_change, 3) 
    orelse
    erlang:function_exported(M, code_change, 4).

appup_for_code_change(M, OldApp, NewApp) ->
    [{update, M, {advanced, [OldApp#app.vsn, NewApp#app.vsn]}}].

appup_for_supervisor(M, OldApp, NewApp) ->
    I = {update, M, supervisor},
    case erlang:function_exported(M, sup_upgrade_notify, 2) of
        true ->
            [I, {apply, {M, sup_upgrade_notify, 
                         [OldApp#app.vsn, NewApp#app.vsn]}}];
        false ->
            [I]
    end.

%% strip down module_info/0 response to the bits we care about
parse_module_info(MI) ->
    Attrs = proplists:get_value(attributes, MI),
    Behaviours = proplists:get_value(behaviour, Attrs, []),
    Exports = proplists:get_value(exports, MI, []),
    [ 
        {behaviours, Behaviours},
        {exports, Exports}
    ].

parse_app_file(Path) ->
    {ok, [{application, AppName, Sections}]} = file:consult(Path),
    Vsn = proplists:get_value(vsn, Sections),
    Mods = proplists:get_value(modules, Sections),
    PathParts = filename:split(Path),
    %% assuming ..../appname/ebin/appname.app strip off /ebin/appname.app
    AppPath = filename:join(lists:sublist(PathParts, length(PathParts)-2)),
    Ebin = filename:join([AppPath, "ebin"]),
    #app{name=AppName, vsn=Vsn, path=AppPath, ebin=Ebin, mods=Mods}.

parse_rel_file(Path, LibsDir, FilterFun) ->
    {ok, [Term]} = file:consult(Path),
    {release, {RelName, RelVsn}, {erts, _ErtsVsn}, AppVers} = Term,
    AppFiles = lists:map(fun({AppNameA,AppVer}) ->
        AppName = atom_to_list(AppNameA),
        filename:join([LibsDir, AppName ++ "-" ++ AppVer, "ebin", AppName ++ ".app"])
    end, AppVers),
    Apps0 = [ parse_app_file(F) || F <- AppFiles ],
    Apps = lists:filter(fun(#app{name=Name}) ->
        FilterFun(atom_to_list(Name))
    end, Apps0),
    {RelName, RelVsn, sort_apps(Apps)}.


%% sort [#app{},...] by name
sort_apps(Apps) -> lists:sort(fun(#app{name=A},#app{name=B}) -> A =< B end, Apps).

%%
%% PRINT HELPERS
%%

print_apps_summary(Apps) ->
    AppsSummary = [
        io_lib:format("~20.. s\t~15.. s\t  ~B\t~s~n",[A#app.name,A#app.vsn,length(A#app.mods),A#app.path])
        || A <- Apps],
    io:format("~s~n",[iolist_to_binary(AppsSummary)]).

print_releases([]) -> ok;
print_releases([Rel|Rels]) ->
    io:format("RELEASE: ~s~nVERSION: ~s~nNUM APPS: ~B~nAPPS:~n",
              [Rel#release.name, Rel#release.vsn, length(Rel#release.apps)]),
    print_apps_summary(Rel#release.apps),
    io:format("~n"),
    print_releases(Rels).


