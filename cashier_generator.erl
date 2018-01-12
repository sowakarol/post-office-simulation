-module(cashier_generator).
-export([generate_cashiers_receiving/0, generate_cashiers_sending/0]).
-import(cashier,[init/1]).

generate_cashier(receive_package) ->
    cashier:init(receive_package);

generate_cashier(send_package) ->
    cashier:init(send_package).

generate_cashiers_receiving() ->
    [generate_cashier(receive_package) 
    || _ <- lists:seq(1,configuration:number_of_cashier_receiving())].

generate_cashiers_sending() ->
    [generate_cashier(send_package) 
    ||  _ <- lists:seq(1,configuration:number_of_cashier_sending())].
