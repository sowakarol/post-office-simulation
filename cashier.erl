-module(cashier).
-compile([export_all]).

-record(cashier, {state = ready, handled_clients=0}).

init()->
    spawn(fun() -> start_working() end).

start_working() -> 
    Self = #cashier{state = ready},
    ready(Self#cashier{state = ready}).

ready(Self = #cashier{state = State, handled_clients = HandleClients}) ->
    io:format("~p~n",[State]),
    % monitor
    receive
        {client, Case, Time} -> 
            io:format("preparing to handle~n"),
            Pid = spawn(fun() -> handle_client() end),
            Pid ! {client, self(), Case, Time},
            ready(Self#cashier{state = busy});
        {get_state, _From} ->
            _From ! {send_state, self(), State},
            ready(Self);
        {handled_client}  ->
            io:format("Cashier handled ~p clients~n", [HandleClients + 1]),
            ready(Self#cashier{state = ready, handled_clients = HandleClients + 1});
        _ ->
            io:format("Not understanding~n"),
            ready(Self)
    end.

%Simulation Time - one minute is equal to 5 seconds
handle_client() ->
    receive
        {client, Pid, Case, Time} ->
            io:format("serving~n"),
            % if not correct case - 30 seconds to tell the client that he needs to go
            % to some specific cashier
            Simulation_Time = Time * 1000 * 5,
            timer:sleep(round(Simulation_Time)),
            io:format("Client was handled after ~p minutes~n", [Time]),
            Pid ! {handled_client}
        end.

take_a_break(Time) ->
    timer:sleep(Time).


terminate(Self = #cashier{handled_clients = X}) ->
    io:format("Cashier was terminated - he handled ~p clients", [X]),
    exit(normal).

