% Acts as an interface to the system. Configures load paths and provides
% predicates for initiating the system.

:- ['../prolongo/load'].
:- use_module(mongo(mongo), []).

:- ['../prolog-json/load'].
:- use_module(json(json), []).

calendar12_configure_globals :-
    % For optimized compiles, tests are by default ignored.
    set_test_options([load(always)]).
    % Try to make everything as UTF-8 as possible.
    % set_prolog_flag(encoding, utf8). % When using streams, global setting.
    % Hunting implicit dependencies is easier without autoload.
    % set_prolog_flag(autoload, false),
    % Displays how modules and such are located.
    % set_prolog_flag(verbose_file_search, true).

calendar12_configure_load_paths :-
    prolog_load_context(directory, Root), % Available only during compilation.
    calendar12_configure_path(Root, 'src', calendar12).

calendar12_configure_path(PathPrefix, PathSuffix, Name) :-
    atomic_list_concat([PathPrefix,PathSuffix], '/', Path),
    asserta(user:file_search_path(Name, Path)).

% Set everything up.
:- calendar12_configure_globals.
:- calendar12_configure_load_paths.

:- include(calendar12(include/common)).

calendar12_load_project_modules :-
    use_module(library(pldoc), []), % Load first to enable comment processing.
    use_module(calendar12(server), []).

calendar12_load_project_tests :-
    plunit:load_test_files([]).

calendar12_server :-
    calendar12_load_project_modules,
    calendar12_load_project_tests,
    calendar12_run_server.

calendar12_test :-
    calendar12_load_project_modules,
    calendar12_load_project_tests,
    calendar12_run_test_suite.

calendar12_run_server :-
    Host = localhost,
    Port = 8080,
    core:format('~n% Connecting to Akka at port ~w ...~n', [Port]),
    server:connect_to_postal_service(Host, Port).

calendar12_run_test_suite :-
    core:format('~n% Run tests ...~n'),
    plunit:run_tests.

calendar12_cov :-
    calendar12_load_project_modules,
    calendar12_load_project_tests,
    calendar12_run_test_suite_with_coverage.

calendar12_run_test_suite_with_coverage :-
    core:format('~n% Run tests ...~n'),
    plunit:show_coverage(plunit:run_tests).

calendar12_repl :-
    calendar12_load_project_modules,
    calendar12_load_project_tests.
