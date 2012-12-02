:- module(_,
    [
        connect_to_postal_service/2
    ]).

:- include(calendar12(include/common)).

:- mutex_create(mongo_mutex).

connect_to_postal_service(Host, Port) :-
    setup_call_catcher_cleanup(
        tcp_socket(Socket),
        tcp_connect(Socket, Host:Port),
        exception(_),
        tcp_close_socket(Socket)),
    setup_call_cleanup(
        tcp_open_socket(Socket, In, Out),
        run_program(postal(In,Out)),
        close_connection(In, Out)).

run_program(postal(PostalIn,PostalOut)) :-
    register_for_actions(postal(PostalIn,PostalOut), [move]),
    setup_call_cleanup(
        (
            mongo:new_connection(Connection),
            mongo:get_database(Connection, 'fnnl_room_service', Database),
            mongo:get_collection(Database, 'rooms', Collection)
        ),
        handle_service(PostalIn, PostalOut, Collection),
        mongo:free_connection(Connection)),
    write(xxxxxxxxxxx).

register_for_actions(postal(_PostalIn,PostalOut), Actions) :-
    json:doc_json(json([actions-Actions]), Payload),
    core:atomic_list_concat(['register: ',Payload], Message),
    send_and_flush(PostalOut, Message).

close_connection(In, Out) :-
    close(In, [force(true)]),
    close(Out, [force(true)]).

handle_service(In, Out, Collection) :-
    receive_line(In, Bytes),
    handle_client_response(In, Out, Collection, Bytes).

handle_client_response(In, Out, Collection, Bytes) :-
    Bytes = [a,c,t,i,o,n,:,' '|BytesJson], % Does not handle 'action:' without trailing ws.
    !,
    core:atom_chars(JsonAtom, BytesJson),
    json:doc_json(Doc, JsonAtom),
    log('Parsed: ~w', [Doc]),
    handle_action_walk(Collection, Doc),
    handle_service(In, Out, Collection).
handle_client_response(In, Out, Collection, Bytes) :-
    log('Unhandled: ~w', [Bytes]),
    handle_service(In, Out, Collection).

handle_action_walk(Collection, Doc) :-
    json_get(Doc, args, [User|_]),
    get_all_rooms(Collection, Rooms),
    log('~w', [Rooms]).

get_all_rooms(Collection, Rooms1) :-
    mongo:find_all(Collection, [], [], Rooms),
    log('weeeeee~w~n', [Rooms]),
    init_rooms(Collection, Rooms, Rooms1).

init_rooms(Collection, [], Rooms1) :-
    !,
    create_rooms(Collection, Rooms1).
init_rooms(_Collection, [RoomsDoc], RoomsList) :-
    bson:doc_get(RoomsDoc, rooms, RoomsList).

create_rooms(Collection, RoomsRev) :-
    create_n_rooms(10, Rooms),
    lists:reverse(Rooms, RoomsRev),
    mongo:upsert(Collection, [rooms-1], [rooms-RoomsRev]).

create_n_rooms(0, []) :- !.
create_n_rooms(N, [[num-N,users-[]]|Rooms]) :-
    N1 is N - 1,
    create_n_rooms(N1, Rooms).

send_and_flush(Socket, Message) :-
    core:format(Socket, '~w~n', [Message]),
    core:flush_output(Socket).

log(Format, Args) :-
    core:atomic_list_concat([Format,'~n'], Format1),
    core:format(Format1, Args),
    core:flush_output.

receive_line(In, Bytes) :-
    core:get_char(In, Byte),
    receive_line_aux(In, Byte, Bytes).

receive_line_aux(In, Byte, []) :-
    stop_char(Byte),
    !,
    core:peek_byte(In, Byte1),
    remove_end_of_line_crap(In, Byte1).
receive_line_aux(In, Byte, [Byte|Bytes]) :-
    receive_line(In, Bytes).

remove_end_of_line_crap(In, Byte) :-
    ( stop_char(Byte) ; whitespace(Byte) ),
    !,
    core:get_char(In, _),
    core:peek_byte(In, Byte2),
    remove_end_of_line_crap(In, Byte2).
remove_end_of_line_crap(_, _).

stop_char('\n').
stop_char('\r').

whitespace(' ').
whitespace('\t').

json_get(json(Pairs), Key, Value) :-
    json_get_aux(Pairs, Key, Value).

json_get_aux([Key-Value|_], Key, Value) :- !.
json_get_aux([_Pair|Pairs], Key, Value) :-
    json_get_aux(Pairs, Key, Value).

% register: {"actions":["move"]}
% action: {"command":"move", "args":["seb","left"]}

% göra register: move till akka
% lyssna på action command move
% skicka event: {message: "marten gick till aoeu"}

/*
[room(1, ["seb","akka"]), room(2, ["mårten"])]
*/









/*

create_server(Port) :-
    tcp_socket(Socket),
    tcp_setopt(Socket, reuseaddr),
    tcp_bind(Socket, Port),
    tcp_listen(Socket, 5),
    tcp_open_socket(Socket, AcceptFd, _),
    dispatch(AcceptFd).

dispatch(AcceptFd) :-
    tcp_accept(AcceptFd, Socket, Peer),
    thread_create(
        process_client(Socket, Peer),
        _,
        [detached(true)]),
    dispatch(AcceptFd).

process_client(Socket, _Peer) :-
    setup_call_cleanup(
        tcp_open_socket(Socket, In, Out),
        setup_call_cleanup(
            mongo:new_connection(Collection),
            handle_service(In, Out, Collection),
            mongo:free_connection(Collection)),
        close_connection(In, Out)).
*/
