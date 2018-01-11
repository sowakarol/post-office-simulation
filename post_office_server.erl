-module(post_office_server).
-compile([export_all]).

% -record(post_office, {gui}).

init() ->
    spawn(fun() -> start_server() end).

start_server() ->
    Workers = generate_cashiers(),
    io:format("~nstarted server with workers with pid ~p~n", [Workers]),
    start_work(Workers).

generate_cashiers() ->
    [cashier:init(cashier_generator:generate_cashier()) || X <- lists:seq(1,configuration:number_of_cashier())].

start_work(Workers) ->
    Work_Start_Time = configuration:working_hours_start_min(),
    Clients = [],
    io:format("starting work ~p~n", [Work_Start_Time]),
    work(Work_Start_Time, Clients, Workers).

work(Day_Time, Clients, Workers) ->
    Day_Time_Plus_10_Minutes = Day_Time + 10,
    timer:sleep(1000),

%%
    Ready_Pids = get_states(Workers, self()),
    io:format("PIDS READY: ~p~n", [Ready_Pids]),

    Clients_Before_Send = get_clients(length(Clients), Clients),
    io:format("CLIENTS BEFORE SEND: ~p~n", [Clients_Before_Send]),
%% foreach klient check Case

    Current_Clients = send_clients(Clients_Before_Send, Ready_Pids, self()),
    io:format("CLIENTS AFTER SEND: ~p~n", [Current_Clients]),

    io:format("TIME: ~p~n", [Day_Time_Plus_10_Minutes]),
    work(Day_Time_Plus_10_Minutes, Current_Clients, Workers).

get_clients(10, Clients) ->
    Clients;
get_clients(_, Clients) ->
    New_Clients = Clients ++ generate_client(),
    get_clients(length(New_Clients), New_Clients).

send_clients([], _, _) ->
    [];
send_clients(Clients, [], _) ->
    Clients;
send_clients([First_Client|Rest_Clients], [First_Pid|Rest_Pids], Self) ->
    io:format("~p", [First_Client]),
    First_Pid ! First_Client,
    send_clients(Rest_Clients, Rest_Pids, Self).

get_states(Workers, Self) ->
    lists:foldl(
        fun(Pid, Ready_Workers) ->
            Pid ! {get_state, Self},
            receive
                {send_state, Pid, ready} ->
                    % io:format("PID ready: ~p~n", [Pid]),
                    New_List = lists:append(Ready_Workers,[Pid]);
                {send_state, Pid, busy} ->
                    % io:format("PID not ready: ~p~n", [Pid]),
                    New_List = Ready_Workers
            end,
            New_List
        end, [], Workers
    ).

generate_client() ->
    [client_generator:generate_client()].

% TODO
% rozroznianie okienek i spraw od klientów
% GUI doszlifowanie - button jakiś do zamykania, pokazywanie statystyk, pokazywanie godziny
% wyświetlenie statystyk
% liczba dni symulacji w konfiguracji
