-module(cashier).
-compile([export_all]).

-behaviour(gen_fsm).

-export([start_link/1]).
-export([init/1, ready/2, handling/2]).
-export([stop/0]).

start_link({Name, Handled_Clients}) -> %later skills, what case can he handle
    gen_fsm:start_link({local, cashier}, cashier, {Name, Handled_Clients}, []).

handle_client({Case, Time}) ->
    gen_fsm:send_event(cashier, {handle_client, {Case, Time}}).

init({Name, Handled_Clients}) ->
    {ok, ready, {[], {Name, Handled_Clients}}}.

ready({handling, {Case, Time}}, {Name, Handled_Clients} ) ->
    %if Case is correct
    {next_state, handling, {Name, Handled_Clients, Case, Time}}.
    
handling({handling, {Case, Time}}, {Name, Handled_Clients}) ->
    Simulation_Time = Time * 1000 * 5,
    timer:sleep(round(Simulation_Time)),
    io:format("Client was handled after ~p minutes~n", [Time]), 
    NewClientsAmount = Handled_Clients + 1,
    io:format("Cashier handled ~p clients~n", [NewClientsAmount]),
    {next_state, ready, {Name, NewClientsAmount}}.

stop() ->
    gen_fsm:send_all_state_event(code_lock, stop).

handle_event(stop, _StateName, StateData) ->
    {stop, normal, StateData}.

terminate(normal, _StateName, _StateData) ->
    ok.

code_change(_, StateName, StateData, _) ->
    terminate(normal,StateName, StateData).


handle_info({'EXIT', _, _}, StateName, StateData) ->
    {next_state, StateName, StateData}.


% -record(cashier, {state = hired, handled_clients=0}).

% init()->
%     spawn(fun() -> start_working() end).

% start_working() -> 
%     Self = #cashier{state = hired},
%     working(Self#cashier{state = working}).

% working(Self = #cashier{state = State}) ->
%     io:format("~p~n",[State]),
%     receive
%         {client, Case, Time} -> 
%             io:format("preparing to handle~n"),
%             handle_client({client, Case, Time}, Self#cashier{state = busy});
%         _ ->
%             io:format("Not understanding~n")
%     end.

% %Simulation Time - one minute is equal to 5 seconds
% handle_client({client, Case, Time}, Self = #cashier{handled_clients = ClientsAmount}  ) ->
%     % if not correct case - 30 seconds to tell the client that he needs to go
%     % to some specific cashier
%     Simulation_Time = Time * 1000 * 5,
%     timer:sleep(round(Simulation_Time)),
%     io:format("Client was handled after ~p minutes~n", [Time]), 
%     NewClientsAmount = ClientsAmount + 1,
%     io:format("Cashier handled ~p clients~n", [NewClientsAmount]),
%     working(Self#cashier{state = working, handled_clients = NewClientsAmount}).

% % get_state() ->

% take_a_break(Time) ->
%     timer:sleep(Time).