-module(cashier).
-compile([export_all]).

-record(cashier, {state = ready, handled_clients = 0, cashierCase = everything}).

init(Case)->
    spawn(fun() -> start_working(Case) end).

start_working(Case) -> 
    Self = #cashier{state = ready, cashierCase = Case},
    ready(Self#cashier{state = ready}).

ready(Self = #cashier{state = State, handled_clients = HandleClients, cashierCase = Case}) ->
    io:format("~p~n",[State]),
    % monitor
    receive
        {client, _Case, Time} -> 
            io:format("preparing to handle~n"),
            Pid = spawn(fun() -> handle_client() end),
            Pid ! {client, self(), _Case, Time},
            ready(Self#cashier{state = busy});
        {get_state, _From, _Case} ->
            if
                Case /= _Case  -> _From ! {send_state, case_different, self(), State};
                true -> _From ! {send_state, case_equal, self(), State}
            end,
            ready(Self);
        {handled_client}  ->
            io:format("Cashier handled ~p clients~n", [HandleClients + 1]),
            ready(Self#cashier{state = ready, handled_clients = HandleClients + 1});
        {terminate, _} ->
            io:format("Terminating cashier"), ok;
        _ ->
            io:format("Not understanding~n"),
            ready(Self)
    end.

%Simulation Time - one minute is equal to 5 seconds
handle_client() ->
    receive
        {client, Pid, _, Time} ->
            io:format("serving~n"),
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

