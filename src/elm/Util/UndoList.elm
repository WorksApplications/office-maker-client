module Util.UndoList
    exposing
        ( UndoList
        , init
        , undo
        , undoWithDiff
        , redo
        , redoWithDiff
        , new
        )


type alias UndoList a =
    { past : List a
    , present : a
    , future : List a
    }


init : a -> UndoList a
init data =
    { past = []
    , present = data
    , future = []
    }


undo : UndoList a -> UndoList a
undo undoList =
    case undoList.past of
        x :: xs ->
            { undoList
                | past = xs
                , present = x
                , future = undoList.present :: undoList.future
            }

        _ ->
            undoList


undoWithDiff : (a -> a -> diff) -> UndoList a -> ( UndoList a, Maybe diff )
undoWithDiff f undoList =
    case undoList.past of
        x :: xs ->
            ( { undoList
                | past = xs
                , present = x
                , future = undoList.present :: undoList.future
              }
            , Just (f x undoList.present)
            )

        _ ->
            ( undoList, Nothing )


redo : UndoList a -> UndoList a
redo undoList =
    case undoList.future of
        x :: xs ->
            { undoList
                | past = undoList.present :: undoList.past
                , present = x
                , future = xs
            }

        _ ->
            undoList


redoWithDiff : (a -> a -> diff) -> UndoList a -> ( UndoList a, Maybe diff )
redoWithDiff f undoList =
    case undoList.future of
        x :: xs ->
            ( { undoList
                | past = undoList.present :: undoList.past
                , present = x
                , future = xs
              }
            , Just (f x undoList.present)
            )

        _ ->
            ( undoList, Nothing )


new : a -> UndoList a -> UndoList a
new a undoList =
    { undoList
        | past = undoList.present :: undoList.past
        , present = a
        , future = []
    }



--
