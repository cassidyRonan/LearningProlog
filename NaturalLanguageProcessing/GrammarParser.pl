

% File PARSER2.PL
% A parser using grammar rule notation

sentence --> noun_phrase, verb_phrase.

noun_phrase --> determiner, noun.
noun_phrase --> noun.

verb_phrase --> auxiliary, verb.
verb_phrase --> verb, noun_phrase.
verb_phrase --> verb, sentence.
verb_phrase --> verb.

determiner --> [the].
determiner --> [a].

auxiliary --> [is].
auxiliary --> [not].

noun --> [fox].
noun --> [foxes].
noun --> [rabbit].
noun --> [rabbits].

verb --> [chase].
verb --> [chased].
verb --> [ran].
verb --> [fast].
verb --> [escaped].
verb --> [hungry].
