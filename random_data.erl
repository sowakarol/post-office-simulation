-module(random_data).
-export([generate/1]).


% to add to average data some possible delays


generate(Average) ->
    random:seed(erlang:timestamp()),
    Percentage = configuration:percentage(),
    Remainder = round(Average * Percentage * 0.01),
    Positive = random:uniform(3),
    if
        Remainder =< 0 -> round(Average);
        true -> case Positive of
            1 -> round(Average) + random:uniform(Remainder);
            2 -> (-1 * random:uniform(Remainder)) + round(Average);
            _ -> round(Average)
        end
    end.
