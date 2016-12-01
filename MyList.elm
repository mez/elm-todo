module MyList exposing (..)


type LinkedList a
    = Empty
    | Node a (LinkedList a)


length : LinkedList a -> Int
length list =
    case list of
        Empty ->
            0

        Node _ next ->
            1 + length next


sum : LinkedList number -> number
sum list =
    case list of
        Empty ->
            0

        Node value next ->
            value + sum next
