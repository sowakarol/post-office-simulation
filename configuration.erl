-module(configuration).
-compile([export_all]).

% post office configuration
number_of_cashier_receiving()   -> 5.   % their duty is to handle clients that want to receive a package
number_of_cashier_sending()     -> 4.   % their duty is to handle clients that want to send a package
working_hours_start()           -> 9.   % 09:00 
working_hours_start_min()       -> working_hours_start() * 60.   % 09:00 
working_hours_end()             -> 17.  % 17:00
working_hours_end_min()         -> working_hours_end() * 60.

% cashier configuration - time in MINUTES
average_time_sending_package()      -> 4.
average_time_receiving_package()    -> 2.
%average_time_answering_question()   -> 1.

%consumers configurations
average_consumers_per_day() -> 100. 

%hesitation's values of random generator in percents
percentage()    -> 10.