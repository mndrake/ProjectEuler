﻿module DeedleFormatter

module DeedleFormatter =

    open System.IO
    open Deedle
    open Deedle.Internal
    open FSharp.Literate
    open FSharp.Markdown
    
    // --------------------------------------------------------------------------------------
    // Implements Markdown formatters for common FsLab things - including Deedle series
    // and frames, F# Charting charts and System.Image values
    // --------------------------------------------------------------------------------------
    // How many columns and rows from frame should be rendered
    let startColumnCount = 3
    let endColumnCount = 3
    let startRowCount = 8
    let endRowCount = 4
    // How many items from a series should be rendered
    let startItemCount = 5
    let endItemCount = 3
    
    // --------------------------------------------------------------------------------------
    // Helper functions etc.
    // --------------------------------------------------------------------------------------
    /// Extract values from any series using reflection
    let (|SeriesValues|_|) (value : obj) = 
        let iser = value.GetType().GetInterface("ISeries`1")
        if iser <> null then 
            let keys = 
                value.GetType().GetProperty("Keys").GetValue(value) :?> System.Collections.IEnumerable
            let vector = value.GetType().GetProperty("Vector").GetValue(value) :?> IVector
            Some(Seq.zip (Seq.cast<obj> keys) vector.ObjectSequence)
        else None
    
    /// Format value as a single-literal paragraph
    let formatValue def = 
        function 
        | Some v -> [ Paragraph [ Literal(v.ToString()) ] ]
        | _ -> [ Paragraph [ Literal def ] ]
    
    /// Format body of a single table cell
    let td v = [ Paragraph [ Literal v ] ]
    
    /// Use 'f' to transform all values, then call 'g' with Some for 
    /// values to show and None for "..." in the middle
    let mapSteps (startCount, endCount) f g input = 
        input
        |> Seq.map f
        |> Seq.startAndEnd startCount endCount
        |> Seq.map (function 
               | Choice1Of3 v | Choice3Of3 v -> g (Some v)
               | _ -> g None)
        |> List.ofSeq
    
    // Tuples with the counts, for easy use later on
    let fcols = startColumnCount, endColumnCount
    let frows = startRowCount, endRowCount
    let sitms = startItemCount, endItemCount
    
    let getHtml (value : obj) = 
        match value with
        | SeriesValues s -> 
            // Pretty print series!
            let heads = 
                s |> mapSteps sitms fst (function 
                         | Some k -> td (k.ToString())
                         | _ -> td " ... ")
            
            let row = 
                s |> mapSteps sitms snd (function 
                         | Some v -> formatValue "N/A" (OptionalValue.asOption v)
                         | _ -> td " ... ")
            
            let aligns = s |> mapSteps sitms id (fun _ -> AlignDefault)
            [ InlineBlock "<div class=\"deedleseries\">"
              TableBlock(Some((td "Keys") :: heads), AlignDefault :: aligns, [ (td "Values") :: row ])
              InlineBlock "</div>" ]
            |> Some
        | :? IFrame as f -> 
            { // Pretty print frame!
              new IFrameOperation<_> with
                  member x.Invoke(f) = 
                      let heads = 
                          f.ColumnKeys |> mapSteps fcols id (function 
                                              | Some k -> td (k.ToString())
                                              | _ -> td " ... ")
                      
                      let aligns = f.ColumnKeys |> mapSteps fcols id (fun _ -> AlignDefault)
                      
                      let rows = 
                          f.Rows
                          |> Series.observationsAll
                          |> mapSteps frows id (fun item -> 
                                 let def, k, data = 
                                     match item with
                                     | Some(k, Some d) -> 
                                         "N/A", k.ToString(), Series.observationsAll d |> Seq.map snd
                                     | Some(k, _) -> 
                                         "N/A", k.ToString(), f.ColumnKeys |> Seq.map (fun _ -> None)
                                     | None -> " ... ", " ... ", f.ColumnKeys |> Seq.map (fun _ -> None)
                                 
                                 let row = 
                                     data |> mapSteps fcols id (function 
                                                 | Some v -> formatValue def v
                                                 | _ -> td " ... ")
                                 
                                 (td k) :: row)
                      Some [ InlineBlock "<div class=\"deedleframe\">"
                             TableBlock(Some([] :: heads), AlignDefault :: aligns, rows)
                             InlineBlock "</div>" ] }
            |> f.Apply
        | _ -> None
        
        |> function
           | Some pars -> 
                let doc = MarkdownDocument(pars, null)
                Markdown.WriteHtml(doc)
           | None -> value.ToString()
    

App.AddFsiPrinter(fun (x:obj) -> 
    App.Kernel.Value.SendDisplayData("text/html", DeedleFormatter.getHtml(box x))
    DeedleFormatter.getHtml(box x))