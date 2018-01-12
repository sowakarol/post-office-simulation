-module(post_office_server).
-compile([export_all]).
-import(cashier_generator,[generate_cashiers_receiving/0,generate_cashiers_receiving/0]).
-import(client_generator,[generate_client/0]).
% -record(post_office, {gui}).

init() ->
    spawn(fun() -> start_server() end).

start_server() ->
    Workers_R = cashier_generator:generate_cashiers_receiving(),
    Workers_S = cashier_generator:generate_cashiers_sending(),
    io:format("~nstarted server with workers with pid ~p ~p~n", [Workers_R, Workers_S]),
    start_work({Workers_R, Workers_S}).


start_work(Workers) ->
    Work_Start_Time = configuration:working_hours_start_min(),
    Clients = {[],[]},
    io:format("starting work ~p~n", [Work_Start_Time]),
    work(Work_Start_Time, Clients, Workers).

work(Day_Time, Clients, Workers = {R,S}) ->
    Day_Time_Plus_10_Minutes = Day_Time + 10,
    timer:sleep(1000),

    Ready_Pids_R = get_states(R, self()),
    io:format("PIDS READY RECEIVE: ~p~n", [Ready_Pids_R]),

    Ready_Pids_S = get_states(S, self()),
    io:format("PIDS READY SENDING: ~p~n", [Ready_Pids_S]),

    {Clients_Before_Send_R, Clients_Before_Send_S} = get_clients(get_clients_length(Clients), Clients),
    io:format("CLIENTS BEFORE SEND: RECEIVING ~p~n", [length(Clients_Before_Send_R)]),
    io:format("CLIENTS BEFORE SEND: SENDING ~p~n", [length(Clients_Before_Send_S)]),
    Clients_Before_Send = {Clients_Before_Send_R, Clients_Before_Send_S},

    NotHandledClients = handle_clients(Clients_Before_Send, Ready_Pids_S, Ready_Pids_R),

    % Current_Clients = send_clients(Clients_Before_Send, Ready_Pids, self()),
    io:format("CLIENTS AFTER SEND: ~p~n", [get_clients_length(NotHandledClients)]),

    io:format("TIME: ~p~n", [Day_Time_Plus_10_Minutes]),
    work(Day_Time_Plus_10_Minutes, NotHandledClients, Workers).

get_clients_length({C_R, C_S}) ->
    length(C_R) + length(C_S).

get_clients(10, Clients) ->
    Clients;

get_clients(_, {C_R, C_S}) ->

    Client = client_generator:generate_client(),
    {client, Case,_} = Client,
    if
        Case == receive_package -> 
            New_Clients = {C_R ++ [Client], C_S};
        true -> 
            New_Clients = {C_R, C_S ++ [Client]}
    end,
    get_clients(get_clients_length(New_Clients), New_Clients).


send_client(Client, []) ->
    Client;

send_client(Client, [First_Pid]) ->
    First_Pid ! Client.

send_clients(Clients, []) ->
    Clients;

send_clients([], _) ->
    [];

send_clients([First_Client|Rest_Clients], [First_Pid|Rest_Pids]) ->
    io:format("~p", [First_Client]),
    send_client(First_Client, [First_Pid]),
send_clients(Rest_Clients, Rest_Pids).

handle_clients({Clients_R, Clients_S}, Ready_Pids_S, Ready_Pids_R) ->
    Unhandled_Clients_R = send_clients(Clients_R, Ready_Pids_R),
    Unhandled_Clients_S = send_clients(Clients_S, Ready_Pids_S),
    {Unhandled_Clients_R, Unhandled_Clients_S}.

    

% send_clients([First_Client = {client, ClientCase, _}|Rest_Clients], [First_Pid_R|Rest_Pids_R],[First_Pid_S|Rest_Pids_S] Self) ->
%     io:format("~p", [First_Client]),
%     if
%         ClientCase == 
%         ClientCase == receive_package -> Client = send_clients(First_Client, )


%     First_Pid ! {check_case, self(), ClientCase},
%     receive
%         {send_case, First_Client, case_different} -> send_clients(First_Client, Rest_Pids, Self);
%         {send_case, First_Client, case_equal} -> io:format("client sent");
%         _ -> io:format("ERROR")
%     end,
%     send_clients(Rest_Clients, Rest_Pids, Self).





get_states(Workers, Self) ->
    io:format("~p ~p", [Workers, Self]),
    lists:foldl(
        fun(Pid, Ready_Workers) ->
            io:format("~p ~p",[Pid,Self]),
            Pid ! {get_state, Self},
            receive
                {send_state, Pid, ready} ->
                    io:format("PID ready: ~p~n", [Pid]),
                    New_List = lists:append(Ready_Workers,[Pid]);
                {send_state, Pid, busy} ->
                    io:format("PID not ready: ~p~n", [Pid]),
                    New_List = Ready_Workers
            end,
            New_List
        end, [], Workers
    ).


% TODO
% rozroznianie okienek i spraw od klientów
% GUI doszlifowanie - button jakiś do zamykania, pokazywanie statystyk, pokazywanie godziny
% wyświetlenie statystyk
% liczba dni symulacji w konfiguracji
