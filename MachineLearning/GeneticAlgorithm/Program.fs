open GeneticAlgorithm
open DomainTypes
open UserInterface
open System

let lenDNA = 8
let numDNA = 100
let rand = new System.Random()
let triangleDistribution = new MathNet.Numerics.Distributions.Triangular(Convert.ToDouble((numDNA-1) * -1),Convert.ToDouble(numDNA-1),0.0)

let createNucleotide() : Nucleotide =
    let upper = rand.Next(10,13)
    let lower = rand.Next(1,9)
    upper <<< 4 ||| lower
    
let createDNA(lenDNA: int): DNA = Array.init lenDNA (fun _ -> createNucleotide())

let createDNAPool(numDNA:int) (lenDNA:int): DNAPool = Array.init numDNA (fun _ -> createDNA lenDNA)  

let breed(parent1:DNA) (parent2:DNA): DNA =
    let crossoverPoint = rand.Next(1,parent1.Length)
    Array.concat[parent1.[..crossoverPoint-1];parent2.[crossoverPoint..]]
    
let decodeNucleotide(value: double) (nucleotide: Nucleotide): double =
    let numeric = nucleotide &&& 0xF
    try
        match (nucleotide >>> 4) with 
        | 10 -> value + Convert.ToDouble numeric
        | 11 -> value - Convert.ToDouble numeric
        | 12 -> value / Convert.ToDouble numeric
        | 13 -> value * Convert.ToDouble numeric
        | _ -> value
    with | _ -> value
    
let decodeDNA(dna: DNA) = dna |> Array.fold (fun acc item -> decodeNucleotide acc item) 0.0

let flipBit(number:int) (position:int): int = (number ^^^ (1 <<< position))

let mutateNucleotide(nucleotide: Nucleotide): Nucleotide =
    let mutable n = nucleotide
    for i = 0 to 7 do
        n <- match (rand.Next(0,1000)) with 
                | 500 -> flipBit n i 
                | _ -> n
    n

let mutateDNA(dna: DNA): DNA = dna |> Array.map mutateNucleotide

let scoreDNA(dna: DNA): double = abs(42.0 - decodeDNA dna)

let rec evolve(pool: DNAPool) (count: Int32) =
    let sortedPool = pool |> Array.sortBy (scoreDNA)
    let totalScore = pool |> Array.sumBy(scoreDNA) 
    printfn "Evolution: %i\tFittest: %f\tTotal:%f\tEquation: %s" count <| (decodeDNA sortedPool.[0]) <| totalScore <| decodeDNAToString sortedPool.[0]
    
    if (scoreDNA sortedPool.[0] >= 0.0 && scoreDNA sortedPool.[0] <= 0.01) then 0
    else
    let child = breed pool.[Convert.ToInt32(abs (triangleDistribution.Sample()))] pool.[Convert.ToInt32(abs (triangleDistribution.Sample()))] |> mutateDNA
    let newPool = sortedPool |> Array.take(numDNA-1) |> Array.append [|child|]
    evolve newPool (count+1)

[<EntryPoint>]
let main argv =
    let pool = createDNAPool numDNA lenDNA
    evolve pool 0 |> ignore
    0