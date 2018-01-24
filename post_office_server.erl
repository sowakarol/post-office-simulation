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
    try
        cf:init(),
        Server = spawn(fun() -> start_server() end),
        io:format("-------------SERV ~p", [Server])
    catch
        Exception:Reason -> {caught, Exception, Reason}
    end.

start_server() ->
    Workers_R = cashier_generator:generate_cashiers_receiving(),
    Workers_S = cashier_generator:generate_cashiers_sending(),
    io:format("~nstarted server with workers with pid ~p ~p~n", [Workers_R, Workers_S]),
    %TODO
    %GUI init GUI with workers
    Server = self(),
    Gui = spawn(fun() -> gui:init({Workers_R, Workers_S}, Server) end),
    sendGuiToWorkers(Workers_R, Gui),
    sendGuiToWorkers(Workers_S, Gui),

    Clock = spawn(fun() -> postOfficeClock() end),
    start_work({Workers_R, Workers_S}, Clock, [],Gui).

sendGuiToWorkers(Workers, Gui) ->
    lists:foreach(fun(Pid) -> 
                    Pid ! {gui, Gui}
                  end, Workers
    ).

%TIMER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
postOfficeClock() ->
    Days = configuration:days(),
    StartTime = configuration:working_hours_start_min(),
    EndTime = configuration:working_hours_end_min(),
    Interval = configuration:one_minute_in_application(),
    startOfficeClock(StartTime,EndTime,Interval, Days).

startOfficeClock(StartTime, EndTime, Interval,Days)->
    receive
        {start_work, OfficePID} ->
            countTime(StartTime, EndTime, Interval, OfficePID, Days)
    end.


countTime(CurrentTime, EndTime, Interval, OfficePID, Days) ->
    if 

        EndTime > CurrentTime -> countOneMinute(CurrentTime,EndTime, Interval, OfficePID, Days);
        EndTime =< CurrentTime ->
            NewDays = Days - 1,
            if
                NewDays == 0    -> OfficePID ! {end_simulation};
                true            -> 
                    OfficePID ! {end_work, self()},
                    startOfficeClock(configuration:working_hours_start_min(),
                                    configuration:working_hours_end_min(),
                                    configuration:one_minute_in_application(),
                                    NewDays)
            end
    end.


countOneMinute(CurrentTime, EndTime,Interval, OfficePID, Days) ->
    % sleep for app's one minute
    io:format("~p~n",[Interval]),
    timer:sleep(round(1000 * Interval)),
    OfficePID ! {time_passed, CurrentTime + 1},
    receive
        {ok, OfficePID} -> ok;
        {kill_clock, OfficePID} -> exit(normal)
    end,
    countTime(CurrentTime + 1, EndTime, Interval, OfficePID, Days).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_work(Workers, Clock, Stats, Gui) ->
    Work_Start_Time = configuration:working_hours_start_min(),
    Clients = {[],[]},
    io:format("starting work ~p~n", [Work_Start_Time]),
    Clock ! {start_work, self()},
    listen(Work_Start_Time, Clients, Workers, Stats, Gui, Clock).


listen(_, Clients, Workers, Stats, Gui, Clock) ->
    receive
        {end_simulation} -> 
            % synchronized(Workers),
            synchronize(Clock),
            summarize(end_simulation,Stats, Workers, Gui);
        {end_work, ClockPID} ->
            % synchronized(Workers),
            Gui ! {end_day},
            NewSt =  summarize(end_day,Workers),
            NewStats = addStats(Stats, NewSt),
            timer:sleep(round(1000*configuration:break_after_day())),
            start_work(Workers, ClockPID, NewStats, Gui);
        {time_passed, CurrentTime} ->
            %TODO
            %GUI update TIME
            Gui ! {time_passed, CurrentTime},
            Clock ! {ok, self()},
            work(CurrentTime, Clients, Workers, Stats, Gui, Clock)
    end.



synchronize(Clock) ->
    Interval = configuration:one_minute_in_application(),
    receive
        {time_passed, _} -> 
            Clock ! {kill_clock, self()}
    after 
        round(Interval * 3) -> ok
    end.


work(Day_Time, Clients, Workers = {R,S}, Stats, Gui, Clock) ->

    Ready_Pids_R = get_states(R, self()),
    % io:format("PIDS READY RECEIVE: ~p~n", [Ready_Pids_R]),

    Ready_Pids_S = get_states(S, self()),
    % io:format("PIDS READY SENDING: ~p~n", [Ready_Pids_S]),

    {Clients_Before_Send_R, Clients_Before_Send_S} = get_clients(new, Clients, Day_Time, Gui),
    io:format("CLIENTS BEFORE SEND: RECEIVING ~p~n", [length(Clients_Before_Send_R)]),
    io:format("CLIENTS BEFORE SEND: SENDING ~p~n", [length(Clients_Before_Send_S)]),
    Clients_Before_Send = {Clients_Before_Send_R, Clients_Before_Send_S},

    NotHandledClients = handle_clients(Clients_Before_Send, Ready_Pids_S, Ready_Pids_R, Gui),

    % Current_Clients = send_clients(Clients_Before_Send, Ready_Pids, self()),
    io:format("CLIENTS AFTER SEND: ~p~n", [get_clients_length(NotHandledClients)]),

    io:format("TIME: ~p~n", [Day_Time]),
    listen(Day_Time, NotHandledClients, Workers, Stats, Gui, Clock).



get_clients_length({C_R, C_S}) ->
    length(C_R) + length(C_S).

get_clients(new, Clients,  DayTime, Gui) ->
    ClientsNumber = client_generator:generate_clients_number(DayTime),
    RandomizedClientsNumber = random_data:generate(ClientsNumber),
    io:format("~p~n", [RandomizedClientsNumber]),
    get_clients(RandomizedClientsNumber, Clients, Gui).

get_clients(0, Clients, Gui) ->
    Clients;

get_clients(X, {C_R, C_S}, Gui) ->
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
    % Gui ! {client, Client},

    Y = X-1,
    get_clients(Y, New_Clients, Gui).


send_client(Client, [], Gui) ->
    Client;

send_client(Client, [First_Pid], Gui) ->
    %TODO
    %GUI send client to worker
    First_Pid ! Client.

send_clients(Clients, [], Gui) ->
    Clients;

send_clients([], _, Gui) ->
    [];

send_clients([First_Client|Rest_Clients], [First_Pid|Rest_Pids], Gui) ->
    io:format("~p~n", [First_Client]),
    send_client(First_Client, [First_Pid], Gui),
send_clients(Rest_Clients, Rest_Pids, Gui).

handle_clients({Clients_R, Clients_S}, Ready_Pids_S, Ready_Pids_R, Gui) ->
    Unhandled_Clients_R = send_clients(Clients_R, Ready_Pids_R, Gui),
    Unhandled_Clients_S = send_clients(Clients_S, Ready_Pids_S, Gui),
    Gui ! {clients, Unhandled_Clients_S, Unhandled_Clients_R},
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
% synchronized(Workers) ->
%     synchronize(Workers).

% synchronize([]) -> ;

% synchronize([First|T]) ->
%     First ! {goodbye, self()},
%     receive
%         {goodbye,First} -> synchronize(T)
%     end.


addStats(List, {NR,NS}) ->
    [{NR,NS}] ++ List.

summarize(end_day,Workers) ->
    collectInformation(end_day, Workers).


summarize(end_simulation,Stats, Workers, Gui) -> 
    %Do sth with Stats
    {DR,DS} = collectInformation(Workers),
    NewStats = addStats(Stats,{DR,DS}),
    io:format("~p~n",[NewStats]),
    Sum = displayStats(NewStats),
    killWorkers(Workers, Sum, Gui).

collectInformation(end_day, {R,S}) ->
    Result = collectInformation({R,S}),
    io:format("Done"),
    Result.

collectInformation({R,S}) ->
    ResultReceived = collect(R,0),
    ResultSent = collect(S,0),
    Result = ResultReceived + ResultSent,
    io:format("~p", [Result]),
    %TODO
    %GUI display results
    io:format("~nEnded day with ~p clients handled~n", [Result]),
    io:format("~nPackages sent ~p~nPackages Received ~p~n",[ResultSent, ResultReceived]),
    {ResultReceived, ResultSent}.


collect([], X) -> X;


collect([First|T], X) ->
    First ! {end_day, self()},
    receive
        {end_of_work,Y,First} -> 
            io:format("~p ~p~n",[First,Y]),
            collect(T,X + Y)
    end.

killWorkers({R,S}, {R_Sum, S_Sum}, Gui) ->
    killWorkers(R,0),
    killWorkers(S,0),

    Gui ! {statistics, {R_Sum, S_Sum}},
    io:format("~nEnded simulation with ~p clients handled~n", [R_Sum + S_Sum]),
    io:format("~nPackages sent ~p~nPackages Received ~p~n",[S_Sum, R_Sum]).

killWorkers([], X) -> X;

killWorkers([First|T], X) ->
    First ! {terminate, self()},
    receive
        {end_of_work,Y,First} -> killWorkers(T,X + Y);
        _ -> killWorkers(T, X)
    end.

displayStats(Stats) ->
    displayStats(Stats, configuration:days(), {0,0}).

displayStats([], _, Sum) -> Sum;

% displayStats([{R,S}], X) ->
%     io:format("On Day ~p : ~p packages received, ~p packages sent", [X, R, S]),
%     displayStats([], X - 1);

displayStats([{R,S}|T], X, {R_Sum, S_Sum}) ->
    io:format("~nOn Day ~p : ~p packages received, ~p packages sent~n", [X, R, S]),
    displayStats(T, X -1, {R_Sum + R, S_Sum + S}).



% TODO
% GUI doszlifowanie - button jakiś do zamykania, pokazywanie statystyk, pokazywanie godziny
% wyświetlenie statystyk
% liczba dni symulacji w konfiguracji
