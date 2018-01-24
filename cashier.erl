-module(cashier).
-compile([export_all]).

-record(cashier, {state = ready, handled_clients = 0, cashierCase = everything, day = 1, gui = gui}).

init(Case)->
    spawn(fun() -> start_working(Case, 1) end).

start_working(Case, Day) -> 
    Self = #cashier{state = ready, cashierCase = Case, handled_clients = 0},
    ready(Self#cashier{state = ready, day = Day}).

start_working(Case, Day, Gui) -> 
    Self = #cashier{state = ready, cashierCase = Case, handled_clients = 0},
    ready(Self#cashier{state = ready, day = Day, gui = Gui}).

ready(Self = #cashier{state = State, handled_clients = HandleClients, cashierCase = Case, day = Day, gui = Gui}) ->
    % monitor
    receive
        {gui, GuiPid} -> 
            io:format("gui ~p", [GuiPid]),
            ready(Self#cashier{gui = GuiPid});
        {client, _Case, Time} -> 
            io:format("preparing to handle~n"),
            Gui ! {cashier, {self(), Case}},
            Pid = spawn(fun() -> handle_client(Gui, self(), Case) end),
            Pid ! {client, self(), _Case, Time, Day},
            ready(Self#cashier{state = busy});
        {get_state, _From} ->
            _From ! {send_state, self(), State},
            ready(Self);   
        {handled_client, D}  ->
            if 
                D /= Day -> 
                    io:format("elo"),
                    Gui ! {cashier, done, {self(), Case}},
                    ready(Self#cashier{state = State});
                true ->
                    Gui ! {cashier, done, {self(), Case}},
                    io:format("Cashier handled ~p clients~n", [HandleClients + 1]),
                    ready(Self#cashier{state = ready, handled_clients = HandleClients + 1})
            end;
        {end_day, _From} ->
            _From ! {end_of_work, HandleClients, self()},
            io:format("sleeeping~n"),
            start_working(Case, Day + 1, Gui);
        % {goodbye, _} ->
        %     {goodbye, self()},
        %     ready(Self).
        {terminate, _From} ->
            _From ! {end_of_work, HandleClients, self()},
            io:format("Terminating cashier~n"), exit(normal);
        W ->
            io:format("Not understanding ~p ~n", [W]),
            ready(Self)
    end.

%Simulation Time - one minute is equal to 5 seconds
handle_client(Gui, ParId, Case) ->
    receive
        {client, Pid, _, Time, Day} ->
            % io:format("serving~n"),
            Simulation_Time = Time * 1000 * configuration:one_minute_in_application(),
            timer:sleep(round(Simulation_Time)),
            % io:format("Client was handled after ~p minutes~n", [Time]),
            Pid ! {handled_client, Day}
        end.

take_a_break(Time) ->
    timer:sleep(Time).


% terminate(Self = #cashier{handled_clients = X}) ->
%     io:format("Cashier was terminated - he handled ~p clients", [X]),
%     exit(normal).

