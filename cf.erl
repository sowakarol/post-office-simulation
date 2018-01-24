% to handle configuration file
-module(cf).
-compile([export_all]).


init() ->
    case configuration:working_hours_start() =< 0 of
        true -> throw(bad_config_working_hours_start);
        false -> ok
    end,
    case configuration:working_hours_end() < 0 of
        true -> throw(bad_config_working_hours_end);
        false -> ok
    end,
    case configuration:working_hours_start() > configuration:working_hours_end() of
        true -> throw(bad_config_working_hours_start_and_end);
        false -> ok
    end,
    case configuration:average_consumers_per_night() < 0  of
        true -> throw(bad_config_consumers_per_night);
        false -> ok
    end,
    case configuration:average_consumers_per_morning() < 0  of
        true -> throw(bad_config_consumers_per_morning);
        false -> ok
    end,
    case configuration:average_consumers_per_noon() < 0  of
        true -> throw(bad_config_consumers_per_noon);
        false -> ok
    end,
    case configuration:average_consumers_per_afternoon() < 0  of
        true -> throw(bad_config_consumers_per_afternoon);
        false -> ok
    end,
    case configuration:average_consumers_per_evening() < 0  of
        true -> throw(bad_config_consumers_per_evening);
        false -> ok
    end,
    case configuration:average_time_receiving_package() =< 0  of
        true -> throw(bad_config_time_receiving_package);
        false -> ok
    end,
    case configuration:average_time_sending_package() =< 0  of
        true -> throw(bad_config_time_sending_package);
        false -> ok
    end,
    case configuration:one_minute_in_application() =< 0.0  of
        true -> throw(bad_config_one_minute_in_app);
        false -> ok
    end,
    case configuration:days() =< 0  of
        true -> throw(bad_config_days);
        false -> ok
    end,
    case configuration:number_of_cashier_receiving() < 0  of
        true -> throw(bad_config_cashier_receiving);
        false -> ok
    end,
    case configuration:number_of_cashier_sending() < 0  of
        true -> throw(bad_config_cashier_sending);
        false -> ok
    end.

number_of_cashier_receiving() ->
    round(configuration:number_of_cashier_receiving()).

number_of_cashier_sending() ->
    round(configuration:number_of_cashier_sending()).


working_hours_start_min() ->
    round(configuration:working_hours_start() * 60).

working_hours_end_min() ->
    round(configuration:working_hours_end() * 60).
