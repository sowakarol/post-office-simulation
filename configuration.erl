-module(configuration).
-compile([export_all]).

% post office configuration
number_of_cashier_receiving()   -> 5.   % their duty is to handle clients that want to receive a package
number_of_cashier_sending()     -> 4.   % their duty is to handle clients that want to send a package
working_hours_start()           -> 9.   % 09:00 
working_hours_end()             -> 10.  % 10:00

% cashier configuration - time in MINUTES
average_time_sending_package()      -> 4.
average_time_receiving_package()    -> 2.
%average_time_answering_question()   -> 1.


% simulation time
one_minute_in_application() -> 4. % in seconds

%consumers configurations
% average number IN ONE HOUR
average_consumers_per_night()       -> 25. %% 00:00 -  6:00 
average_consumers_per_morning()     -> 65. %%  6:00 - 11:00 
average_consumers_per_noon()        -> 55. %% 11:00 - 14:00
average_consumers_per_afternoon()   -> 75. %% 14:00 - 19:00
average_consumers_per_evening()     -> 50. %% 19:00 - 24:00


%hesitation's values of random generator in percents
percentage()    -> 10.

%%
working_hours_start_min()       -> working_hours_start() * 60.   % 09:00 
working_hours_end_min()         -> working_hours_end() * 60.
