#I "C:/Program Files/BayardRock/IFSharp/lib"
#r "FSharp.Data.dll"

open FSharp.Data

let wb = WorldBankData.GetDataContext()

wb.Countries