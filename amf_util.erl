-module(amf_util).

-compile(export_all).

object_to_record(TupleList, [Prop|PropList]) ->
        {_, Prop1} = Prop,
        object_to_record([Prop1|TupleList], PropList);

object_to_record(TupleList, []) ->
        list_to_tuple(lists:reverse(TupleList)).

object_to_record(Object) ->
        {object, RecName, PropList} = Object,
        TupleList = [binary_to_atom(RecName, utf8)],
        object_to_record(TupleList, PropList).

