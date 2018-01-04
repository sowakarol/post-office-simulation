-module(configuration).
-compile([export_all]).



% maybe it will be changed later to xmerl

% post office configuration
number_of_cashier()         -> 5.
working_hours_start()       -> 9.   % 09:00 
working_hours_end()         -> 17.  % 17:00


% cashier configuration - time in MINUTES
average_time_sending_package()      -> 4.
average_time_receiving_package()    -> 2.
average_time_answering_question()   -> 1.

%consumers configurations
average_consumers_per_day() -> 100. 
