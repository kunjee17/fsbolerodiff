module FSBolero.Client.Main

open Elmish
open Bolero
open Bolero.Html
open DiffSharp

type Model = { currentTensor: Tensor }

let initModel = { currentTensor = Tensor.Zero }

type Message =
    | Zero
    | One
    | Scalar
    | Vector

let init arg =
    let t1 = Tensor.Zero
    { currentTensor = t1 }

let update message model =
    match message with
    | Zero ->
        { model with
              currentTensor = Tensor.Zero }
    | One ->
        { model with
              currentTensor = Tensor.One }
    | Scalar ->
        { model with
              currentTensor = dsharp.tensor 1.2 }
    | Vector ->
        { model with
              currentTensor = (dsharp.tensor [ 0.0; 0.3; 0.1 ]) }

// Define a scalar-to-scalar function
let f (x: Tensor) = sin (sqrt x)

// Get its derivative
let df = dsharp.diff f

// Now define a vector-to-scalar function
let g (x: Tensor) = exp (x.[0] * x.[1]) + x.[2]

// Now compute the gradient of g
let gg = dsharp.grad g

// Compute the hessian of g
let hg = dsharp.hessian g

let view (model: Model) dispatch =
    div [ attr.classes [ "container" ] ] [
        div [ attr.classes [ "columns" ] ] [
            div [ attr.classes [ "column" ] ] []
            div [ attr.classes [ "column" ] ] [
                text (sprintf "Current Tensor %A, and Shape of it %A" model.currentTensor model.currentTensor.shape)
            ]
            div [ attr.classes [ "column" ] ] []
        ]

        if (model.currentTensor.shape.Length = 0) then
            div [ attr.classes [ "columns" ] ] [
                div [ attr.classes [ "column" ] ] []
                div [ attr.classes [ "column" ] ] [
                    text (sprintf "sin (sqrt x) of scalar: %A" (f (model.currentTensor)))
                ]
                div [ attr.classes [ "column" ] ] []
            ]

            div [ attr.classes [ "columns" ] ] [
                div [ attr.classes [ "column" ] ] []
                div [ attr.classes [ "column" ] ] [
                    text (sprintf "diff of sin (sqrt x) scalar: %A" (df (model.currentTensor)))
                ]
                div [ attr.classes [ "column" ] ] []
            ]

        if (model.currentTensor.shape.Length = 1) then
            div [ attr.classes [ "columns" ] ] [
                div [ attr.classes [ "column" ] ] []
                div [ attr.classes [ "column" ] ] [
                    text (sprintf "exp (x.[0] * x.[1]) + x.[2] vector: %A" (g (model.currentTensor)))
                ]
                div [ attr.classes [ "column" ] ] []
            ]

            div [ attr.classes [ "columns" ] ] [
                div [ attr.classes [ "column" ] ] []
                div [ attr.classes [ "column" ] ] [
                    text (sprintf "gradient of exp (x.[0] * x.[1]) + x.[2] vector: %A" (gg (model.currentTensor)))
                ]
                div [ attr.classes [ "column" ] ] []
            ]

            div [ attr.classes [ "columns" ] ] [
                div [ attr.classes [ "column" ] ] []
                div [ attr.classes [ "column" ] ] [
                    text (sprintf "hessian of exp (x.[0] * x.[1]) + x.[2] vector: %A" (hg (model.currentTensor)))
                ]
                div [ attr.classes [ "column" ] ] []
            ]

        div [ attr.classes [ "columns" ] ] [
            div [ attr.classes [ "column" ] ] []
            div [ attr.classes [ "column" ] ] [
                div [ attr.classes [ "columns" ] ] [
                    div [ attr.classes [ "column" ] ] [
                        button [ attr.classes [ "button" ]
                                 on.click (fun _ -> Zero |> dispatch) ] [
                            text "Zero"
                        ]
                    ]
                    div [ attr.classes [ "column" ] ] [
                        button [ attr.classes [ "button" ]
                                 on.click (fun _ -> One |> dispatch) ] [
                            text "One"
                        ]
                    ]
                    div [ attr.classes [ "column" ] ] [
                        button [ attr.classes [ "button" ]
                                 on.click (fun _ -> Scalar |> dispatch) ] [
                            text "Scalar 1.2"
                        ]
                    ]

                    div [ attr.classes [ "column" ] ] [
                        button [ attr.classes [ "button" ]
                                 on.click (fun _ -> Vector |> dispatch) ] [
                            text "Vector [ 0.0; 0.3; 0.1 ]"
                        ]
                    ]
                ]
            ]
            div [ attr.classes [ "column" ] ] []
        ]
    ]


type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program = Program.mkSimple (init) update view
