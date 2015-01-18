-module(appupper).
-compile(export_all).
-export([main/1]).
-include("appupper.hrl").
-record(app, {name, vsn, path, ebin, mods=[]}).
-record(release, {name, vsn, apps=[]}).

-define(otp_apps, [
    asn1, common_test, compiler, cosEvent, cosEventDomain, cosFileTransfer,
    cosNotification, cosProperty, cosTime, cosTransactions, crypto, debugger,
    dialyzer, diameter, edoc, eldap, erl_docgen, erl_interface, erts, et,
    eunit, gs, hipe, ic, inets, jinterface, kernel, megaco, mnesia, observer,
    odbc, orber, os_mon, ose, otp_mibs, parsetools, percept, public_key,
    reltool, runtime_tools, sasl, snmp, ssh, ssl, stdlib, syntax_tools,
    test_server, tools, typer, webtool, wx, xmerl
]).

%% Design notes:
%% 1) Construct two #releases{} containing a list of #app{} each
%% 2) Compare apps from the two releases, and generate .appups
%% 3) Profit.

main(Args) ->
    case appupper_cli:parse_args(Args) of
        {ok, State = #state{}} -> run(State);
        _ -> init:stop(1)
    end.

%-record(state, {
        %relname
    %,   relpath
    %,   upfrom
    %,   relvsn
%}).

stderr(S) -> stderr(S,[]).
stderr(S,A) -> io:put_chars(standard_error, [io_lib:format(S,A),"\n"]).

run(State = #state{}) ->
    io:format("# RUN ~p~n",[State]),
    try
        run_directives(State#state.directives, State)
    catch
        throw:{err, S, A} ->
            stderr(S,A),
            init:stop(1)
    end.

run_directives([], State) ->
    State;

run_directives([appups|Directives], State) ->
    Diff = diff_rels(State),
    io:format("~p\n",[Diff]),
    run_directives(Directives, State);

run_directives([listrels|Directives], State) ->
    Rels = list_releases(State),
    io:format("~p\n",[Rels]),
    run_directives(Directives, State).

list_releases(#state{relpath=Relpath}) ->
    Dir = Relpath ++ "/releases/",
    case file:list_dir("_rel/relsandbox/releases") of
        {ok, Files} ->
            lists:reverse(lists:foldl(fun(F, Acc) ->
                Path = Dir ++ F,
                case filelib:is_dir(Path) of
                    true -> [{F, Path}|Acc];
                    false -> Acc
                end
            end, [], Files));
        {error, _Err} ->
            throw({err, "No releases dir found @ ~s", [Dir]})
    end.

diff_rels(State=#state{upfrom=OldV,relname=RelName}) ->
    OldRelPath = filename:join([State#state.relpath,"releases",OldV, RelName ++ ".rel"]),
    io:format("oldrelpath: ~s\n",[OldRelPath]),
    FilterFun = fun(_) -> true end,
    {RelName, OldVsn, OldApps} = parse_rel_file(OldRelPath, "_rel/"++RelName++"/lib", FilterFun),
    io:format("OLD vsn: ~p apps: ~p\n",[OldVsn, OldApps]),
    ok.

%read_appfile_props(File) ->
    %{ok, [{application, _AppName, Props}]} = file:consult(File),
    %Props.

%% working copy vs last _rel version
xmain([RelVsn]) ->
    {ok, [[{release, RelName, _RelVer, _, _, _}]]} = file:consult("_rel/releases/RELEASES"),
    OldRelPath = filename:join(["_rel","releases",RelVsn, RelName ++ ".rel"]),
    FilterFun = make_appname_filter(),
    {RelName, OldVsn, OldApps} = parse_rel_file(OldRelPath, "_rel/lib", FilterFun),
    OldRel = #release{name=RelName, vsn=OldVsn, apps=OldApps},
    NewApps = parse_local_appvers(["apps"]),
    NewRel = OldRel#release{vsn="new", apps=NewApps},
    compare_releases(OldRel, NewRel),
    ok;

xmain([OldV, NewV]) ->
    {ok, [[{release, RelName, _RelVer, _, _, _}]]} = file:consult("_rel/releases/RELEASES"),
    FilterFun = make_appname_filter(),
    OldRelPath = filename:join(["_rel","releases",OldV, RelName ++ ".rel"]),
    NewRelPath = filename:join(["_rel","releases",NewV, RelName ++ ".rel"]),
    {RelName, OldVsn, OldApps} = parse_rel_file(OldRelPath, "_rel/lib", FilterFun),
    {RelName, NewVsn, NewApps} = parse_rel_file(NewRelPath, "_rel/lib", FilterFun),
    OldRel = #release{name=RelName, vsn=OldVsn, apps=OldApps},
    NewRel = #release{name=RelName, vsn=NewVsn, apps=NewApps},
    compare_releases(OldRel, NewRel),
    ok;

xmain(_) ->
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
    %% only make appups for apps that are in the old AND new releases
    Apps = lists:filter(fun(A) ->
        proplists:get_value(A#app.name, OldApps) =/= undefined
    end, NewRel#release.apps),
    AppUpDirectives = lists:map(fun(App) ->
        OldApp = proplists:get_value(App#app.name, OldApps),
        {App#app.name, OldApp#app.vsn, App#app.vsn, App, make_appup(OldApp, App)}
    end, Apps),
    process_appup_directives(AppUpDirectives).

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
    case UpInstructions of
        [] when OldApp#app.vsn =:= NewApp#app.vsn ->
            not_upgraded;
        _  when OldApp#app.vsn =:= NewApp#app.vsn ->
            {needs_version_bump, UpInstructions};
        _ ->
            AppUpPath = filename:join([NewApp#app.path, "ebin", 
                                       atom_to_list(NewApp#app.name) ++ ".appup"]),
            DownInstructions = reverse_appup_instructions(UpInstructions),
            {appup, AppUpPath,
                {NewApp#app.vsn,
                    [{OldApp#app.vsn, UpInstructions}],
                    [{OldApp#app.vsn, DownInstructions}]}
            }
    end.

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
        [ appup_instruction_for_module(M, OldApp, NewApp) || M <- sort_mods(SameMods) ],
        [ {delete_module, M} || M <- RemovedMods ]
    ]).

%% Sort _sup modules first
sort_mods(Mods) ->
    lists:sort(
        fun(A,B) ->
                As = atom_to_list(A),
                Bs = atom_to_list(B),
                case {lists:suffix("_sup", As), lists:suffix("_sup", Bs)} of
                    {true, false} -> true;
                    {false, true} -> false;
                    _             -> A =< B
                end
        end,
    Mods).

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
        {error, beam_lib, _ErrRsn} ->
            %io:format("beam_lib:cmp(~p) ~p~n",[M, ErrRsn]),
            Minfo = extract_module_info(NewBeam),
            HasBehaviour = fun(B) ->
                lists:member(B, proplists:get_value(behaviours, Minfo, []))
            end,
            HasCodeChange = fun() ->
                lists:member({code_change, 3}, Minfo) 
                orelse
                lists:member({code_change, 4}, Minfo)
            end,
            Instructions = case HasBehaviour(supervisor) of
                true  -> 
                    appup_for_supervisor(M, Minfo, OldApp, NewApp);
                false ->
                    %% gen_server has code_change/3
                    %% gen_fsm has code_change/4
                    case HasCodeChange() of
                        true  -> appup_for_code_change(M, OldApp, NewApp);
                        false -> [{load_module, M}]
                    end
            end,
            Instructions
    end.

appup_for_code_change(M, OldApp, NewApp) ->
    [{update, M, {advanced, [OldApp#app.vsn, NewApp#app.vsn]}}].

appup_for_supervisor(M, Minfo, OldApp, NewApp) ->
    I = {update, M, supervisor},
    %% Support the Dukes of Erl erlrc convention:
    case lists:member({sup_upgrade_notify, 2}, 
            proplists:get_value(exports, Minfo, [])) of
        true ->
            [I, {apply, {M, sup_upgrade_notify, 
                         [OldApp#app.vsn, NewApp#app.vsn]}}];
        false ->
            [I]
    end.

extract_module_info(Beam) ->
    Chunker = fun(K) ->
        case beam_lib:chunks(Beam, [K]) of
            {ok, {_, [{K, Result}]}} -> Result;
            _ -> []
        end
    end,
    Exports = Chunker(exports),
    %% Implement the "special relationship"
    Behaviours = lists:usort(
                    lists:flatten(
                        proplists:get_value(behaviour, Chunker(attributes), []) ++ 
                        proplists:get_value(behavior,  Chunker(attributes), []))),
    [
        {behaviours, Behaviours},
        {exports, Exports}
    ].

parse_app_file(Path) ->
    io:format("parse_app_file ~s\n",[Path]),
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
    io:format("APPFILES: ~p\n",[AppFiles]),
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


process_appup_directives(L) ->
    [ process_appup_directive(D) || D <- L ].

process_appup_directive({N, Va, Va, _App, not_upgraded}) ->
    io:format("App '~s' version '~s' - not upgraded, no changes needed.~n",[N, Va]);

process_appup_directive({N, Va, Va, App, {needs_version_bump, UpInstructions}}) ->
    io:format("App '~s' version '~s' - changes detected, needs version bump.~n",[N, Va]),
    AppFile = App#app.ebin ++ "/" ++ atom_to_list(App#app.name) ++ ".app",
    {ok, Vn} = bump_app_version(AppFile),
    DownInstructions = reverse_appup_instructions(UpInstructions),
    Directive = {N, Va, Vn, App#app{vsn=Vn}, 
                    {appup, AppFile ++ "up",
                        {Vn,
                            [{Va, UpInstructions}],
                            [{Va, DownInstructions}]}
                    }},
    process_appup_directive(Directive);

process_appup_directive({N, Va, Vb, _App, {appup, Path, Contents}}) ->
    io:format("~s.appup generated for ~s --> ~s~n",[N, Va, Vb]),
    io:format("~p~n", [Contents]),
    case filelib:is_file(Path) of
        true ->
            {ok, Terms} = file:consult(Path),
            case lists:member(Contents, Terms) of
                true ->
                    io:format("Identical directive already in file.~n");
                false ->
                    NewContents = prep_appfile_contents([ 
                            Contents | remove_appup(Terms, Va, Vb) ]),
                    ok = file:write_file(Path, NewContents),
                    io:format("Wrote: ~s~n", [Path])
            end;
        false ->
            ok = file:write_file(Path, prep_appfile_contents([Contents]))
    end.

prep_appfile_contents(Terms) ->
    [
        io_lib:format("~p.~n",[C]) 
        || C <- Terms
    ].

remove_appup(Terms, Va, Vb) ->
    lists:filter(fun ({A, [{B,_}], [{B,_}]}) -> 
                        not (A =:= Va andalso B =:= Vb);
                     (_) -> 
                        true 
                 end,
                 Terms).

%% increments app version in-place, in app file
bump_app_version(Path) ->
    {ok, [{application, AppName, Sections}]} = file:consult(Path),
    Vsn = proplists:get_value(vsn, Sections),
    NewVsn = next_version(Vsn),
    NewSections = [{vsn, NewVsn} | proplists:delete(vsn, Sections)],
    Contents = io_lib:format("~p.~n",[{application, AppName, NewSections}]),
    io:format("rewrite ~s and change vsn: ~s --> ~s~n",[Path,Vsn,NewVsn]),
    ok = file:write_file(Path, Contents),
    %% Now replace the vsn in the .app.src file, if present:
    Parts = filename:split(Path),
    AppSrcPath = filename:join(lists:sublist(Parts, length(Parts)-2)) ++ "/src/" ++ atom_to_list(AppName)++".app.src",
    case filelib:is_file(AppSrcPath) of
        true ->
            io:format("rewrite ~s and change vsn: ~s --> ~s~n",[AppSrcPath, Vsn, NewVsn]),
            {ok, [{application, AppName, SrcSections}]} = file:consult(AppSrcPath),
            SrcNewSections = [{vsn, NewVsn} | proplists:delete(vsn, SrcSections)],
            SrcContents = io_lib:format("~p.~n",[{application, AppName, SrcNewSections}]),
            ok = file:write_file(AppSrcPath, SrcContents);
        false ->
            io:format("skipping: ~s~n",[AppSrcPath]),
            ok
    end,
    {ok, NewVsn}.

next_version(Vsn) ->
    YYYYMMDD = yyyymmdd(),
    case Vsn of
        [YYYYMMDD | P] ->
            YYYYMMDD ++ [hd(P)+1];
        _ ->
            YYYYMMDD ++ "a"
    end.

yyyymmdd() ->
    {{Y,M,D},_} = calendar:local_time(),
    lists:flatten(io_lib:format("~4..0B~2..0B~2..0b",[Y,M,D])).
