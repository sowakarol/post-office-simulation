-module(post_office_server).
-compile([export_all]).
-import(cashier_generator,[generate_cashiers_receiving/0,generate_cashiers_receiving/0]).
-import(client_generator,[generate_client/0]).


%%TODO
%config not working with non positive values
%liczba dni - statystyki na dzień - liczba klientów
% received X packages, sent Y packages

%przerobienie na aplikacje
%refactor timera
%GUI

init() ->
    Server = spawn(fun() -> start_server() end),
    io:format("-------------SERV ~p", [Server]).

start_server() ->
    Workers_R = cashier_generator:generate_cashiers_receiving(),
    Workers_S = cashier_generator:generate_cashiers_sending(),
    io:format("~nstarted server with workers with pid ~p ~p~n", [Workers_R, Workers_S]),
    %TODO
    %GUI init GUI with workers
    Server = self(),
    Gui = spawn(fun() -> gui:init(5, Server) end),
    Clock = spawn(fun() -> postOfficeClock() end),
    start_work({Workers_R, Workers_S}, Clock, Gui).


postOfficeClock() ->
    StartTime = configuration:working_hours_start_min(),
    EndTime = configuration:working_hours_end_min(),
    Interval = configuration:one_minute_in_application(),
    receive
        {start_work, OfficePID} ->
            countTime(StartTime, EndTime, Interval, OfficePID)
    end.


countTime(CurrentTime, EndTime, Interval, OfficePID) ->
    if 
        EndTime > CurrentTime -> countOneMinute(CurrentTime,EndTime, Interval, OfficePID);
        EndTime =< CurrentTime -> OfficePID ! {end_work}
    end.


countOneMinute(CurrentTime, EndTime,Interval, OfficePID) ->
    % sleep for app's one minute
    io:format("~p~n",[Interval]),
    timer:sleep(round(1000 * Interval)),
    OfficePID ! {time_passed, CurrentTime + 1},
    countTime(CurrentTime + 1, EndTime, Interval, OfficePID).


start_work(Workers, Clock, Gui) ->
    Work_Start_Time = configuration:working_hours_start_min(),
    Clients = {[],[]},
    io:format("starting work ~p~n", [Work_Start_Time]),
    Clock ! {start_work, self()},
    listen(Work_Start_Time, Clients, Workers, Gui).




listen(_, Clients, Workers, Gui) ->
    receive
        {end_work} -> 
            summarize(Workers, Gui),
            io:format("KONIEC -----------------");
        {time_passed, CurrentTime} ->
            %TODO
            %GUI update TIME
            Gui ! {time_passed, CurrentTime},
            work(CurrentTime, Clients, Workers, Gui)
    end.



work(Day_Time, Clients, Workers = {R,S}, Gui) ->

    Ready_Pids_R = get_states(R, self()),
    % io:format("PIDS READY RECEIVE: ~p~n", [Ready_Pids_R]),

    Ready_Pids_S = get_states(S, self()),
    % io:format("PIDS READY SENDING: ~p~n", [Ready_Pids_S]),

    {Clients_Before_Send_R, Clients_Before_Send_S} = get_clients(new, Clients, Day_Time),
    io:format("CLIENTS BEFORE SEND: RECEIVING ~p~n", [length(Clients_Before_Send_R)]),
    io:format("CLIENTS BEFORE SEND: SENDING ~p~n", [length(Clients_Before_Send_S)]),
    Clients_Before_Send = {Clients_Before_Send_R, Clients_Before_Send_S},

    NotHandledClients = handle_clients(Clients_Before_Send, Ready_Pids_S, Ready_Pids_R),

    % Current_Clients = send_clients(Clients_Before_Send, Ready_Pids, self()),
    io:format("CLIENTS AFTER SEND: ~p~n", [get_clients_length(NotHandledClients)]),

    io:format("TIME: ~p~n", [Day_Time]),
    listen(Day_Time, NotHandledClients, Workers, Gui).



get_clients_length({C_R, C_S}) ->
    length(C_R) + length(C_S).

get_clients(new, Clients,  DayTime) ->
    ClientsNumber = client_generator:generate_clients_number(DayTime),
    RandomizedClientsNumber = random_data:generate(ClientsNumber),
    io:format("~p~n", [RandomizedClientsNumber]),
    get_clients(RandomizedClientsNumber, Clients).

get_clients(0, Clients) ->
    Clients;

get_clients(X, {C_R, C_S}) ->
    Client = client_generator:generate_client(),
    {client, Case,_} = Client,
    if
        Case == receive_package -> 
            New_Clients = {C_R ++ [Client], C_S};
        true -> 
            New_Clients = {C_R, C_S ++ [Client]}
    end,
    %TODO
    %GUI show client that arrived
    Y = X-1,
    get_clients(Y, New_Clients).


send_client(Client, []) ->
    Client;

send_client(Client, [First_Pid]) ->
    %TODO
    %GUI send client to worker
    First_Pid ! Client.

send_clients(Clients, []) ->
    Clients;

send_clients([], _) ->
    [];

send_clients([First_Client|Rest_Clients], [First_Pid|Rest_Pids]) ->
    io:format("~p~n", [First_Client]),
    send_client(First_Client, [First_Pid]),
send_clients(Rest_Clients, Rest_Pids).

handle_clients({Clients_R, Clients_S}, Ready_Pids_S, Ready_Pids_R) ->
    Unhandled_Clients_R = send_clients(Clients_R, Ready_Pids_R),
    Unhandled_Clients_S = send_clients(Clients_S, Ready_Pids_S),
    {Unhandled_Clients_R, Unhandled_Clients_S}.


get_states(Workers, Self) ->
    % io:format("~p ~p", [Workers, Self]),
    lists:foldl(
        fun(Pid, Ready_Workers) ->
            % io:format("~p ~p",[Pid,Self]),
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


summarize(Workers, Gui) -> 
    collectInformation(Workers, Gui).

collectInformation({R,T}, Gui) ->
    Result = collect(R,0) + collect(T,0),
    %TODO
    %GUI display results
    Gui ! {statistics, Result},
    io:format("~nEnded simulation with ~p clients handled~n", [Result]).


collect([], X) -> X;

collect([First|T], X) ->
    First ! {terminate, self()},
    receive
        {end_of_work,Y,First} -> collect(T,X + Y);
        _ -> collect(T, X)
    end.


% TODO
% GUI doszlifowanie - button jakiś do zamykania, pokazywanie statystyk, pokazywanie godziny
% wyświetlenie statystyk
% liczba dni symulacji w konfiguracji
