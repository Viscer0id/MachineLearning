module GeneticAlgorithm.UserInterface

open DomainTypes
open System

let nucleotideToString(nucelotide: Nucleotide): string = Convert.ToString(nucelotide, 2).PadLeft(8, '0');

let dnaToString(dna: DNA): string = dna |> Array.map (fun x -> nucleotideToString x) |> Array.reduce (fun acc item -> acc + " " + item)

let printDNAPool(dnaPool: DNAPool) = dnaPool |> Seq.iter (fun x -> printf "%s\n" (dnaToString x))

let decodeNucleotideString(nucleotide: Nucleotide) =
    let operand = match (nucleotide >>> 4) with | 10 -> "+" | 11 -> "-" | 12 -> "/" | 13 -> "x" | _ -> "?"
    let numeric = nucleotide &&& 0xF
    sprintf "%s %i" operand numeric
    
let decodeDNAToString(dna: DNA) = dna |> Array.map (fun x -> sprintf "%s" <| decodeNucleotideString x) |> Array.reduce (fun x y -> x+" "+y)