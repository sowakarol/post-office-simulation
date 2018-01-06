-module(cashier).
-compile([export_all]).

-record(cashier, {state = hired, handled_clients=0}).

init()->
    spawn(fun() -> start_working() end).

start_working() -> 
    Self = #cashier{state = hired},
    working(Self#cashier{state = working}).

working(Self = #cashier{state = State}) ->
    io:format("~p~n",[State]),
    receive
        {client, Case, Time} -> 
            io:format("preparing to handle~n"),
            handle_client({client, Case, Time}, Self#cashier{state = busy});
        _ ->
            io:format("Not understanding~n")
    end.

%Simulation Time - one minute is equal to 5 seconds
handle_client({client, Case, Time}, Self = #cashier{handled_clients = ClientsAmount}  ) ->
    % if not correct case - 30 seconds to tell the client that he needs to go
    % to some specific cashier
    Simulation_Time = Time * 1000 * 5,
    timer:sleep(round(Simulation_Time)),
    io:format("Client was handled after ~p minutes~n", [Time]), 
    NewClientsAmount = ClientsAmount + 1,
    io:format("Cashier handled ~p clients~n", [NewClientsAmount]),
    working(Self#cashier{state = working, handled_clients = NewClientsAmount}).

% get_state() ->

take_a_break(Time) ->
    timer:sleep(Time).