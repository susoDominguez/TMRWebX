print_list(List) :-
    with_output_to(codes(Codes), write(List)),
    format("~s", [Codes]).

remove_head([_|Tail], Tail).

without_last([_], []).
without_last([X|Xs], [X|WithoutLast]) :-
    without_last(Xs, WithoutLast).

remove_file_extension(File, Filename) :-
    split_string(File, '.', '', FileComponents),
    nth0(0, FileComponents, Filename).

getenv_or_default(Name, Default, Value) :-
    (   getenv(Name, Raw), Raw \== ''
    ->  Value = Raw
    ;   Value = Default
    ).

ensure_trailing_slash(Source0, Normalized) :-
    (   atom(Source0)
    ->  Source = Source0
    ;   string(Source0)
    ->  atom_string(Source, Source0)
    ;   throw(error(type_error(atom_or_string, Source0), ensure_trailing_slash/2))
    ),
    (   sub_atom(Source, _, 1, 0, '/')
    ->  Normalized = Source
    ;   atom_concat(Source, '/', Normalized)
    ).
