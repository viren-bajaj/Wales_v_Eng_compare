# Wales_v_Eng_compare

Live version of calculator (v1.1.5): https://virenb.shinyapps.io/Welsh_v_English_contract_compare/

A new contract was negotiated by the welsh resident doctors comittee (WRDC) and NHS wales. DOI - I'm on the committee as of October 2025, but joined after the contract negotiation was concluded, so I had no part in those contract negotiations. Everything included here is soley using public available information.

# This is a hobby project. It is not official from the BMA.

This project compares the Welsh 2002 to proposed Welsh 2026 and the English 2016 doctor contracts. 

The main points of the new contract are a move away from banding to paying for hours worked, plus an out of hours enhancement of 1.5x.
Note that GPSTs on GP blocks get a pay premia, as do OMFS trainees (unclear if welsh OMFS trainees can get this in F1/2).

The new contract retains incremental credit, but adjusts the pay scales to have 5 ST pay nodes rather than 10. 

Whilst this project can calculate NHS pension deductions, I offer no pension advice.
The project can also calculate student loan deductions. 

If required I can add in code for scottish taxpayers (this exists in one of my other repositories), however this is unlikely to be necessary since the purpose of this particular calculator is to assess welsh pay, and it is extremely unlikely someone would be resident in Scotland whilst on Welsh pay. However it could be useful for someone considering applying to Wales for training who currently lives on the Scottish side of the border, but works in England.

v1.0.9 has an error (wrong pay premia data for England), pls use v1.1.0 or newer if you want to use this as basis for your own code for 2025-26 

# toDo:

Change the incremental credit buttons to a slider asking how much incremental credit you had at the start of the relevent raining year so it can calculate your pay if you increment half way through a training year. e.g. You say you're ST1, it asks how much incremental credit you had at the start of ST1 - you chose 1.5 years. This should add 0.5 years of incremental credit of 1, and 0.5 years of incremental credit of 2. If you picked 1.75 years, then 0.25 years of incremental credit of 1, and 0.75 years of incremental credit of 2. 

make the premia selection less ugly
add LTFT (need to clarify how LTFT works on the prposed contract before it is added back in, else I'll have errors)

Add Scottish taxes back in (note that scottish taxes are really weird to calculate - I have found thatmultiple different reputable websites all give slightly different numbers to each other when calculating scottish taxes, and I don't know why)

Consider adding comparisons to NI and Scotland 2002 contracts (banding should be calculated in the same way as on the Welsh 2002 contract, but base pay may be different. However I believe the scottish pension scheme is different from the welsh/english one, so I would have to make a different pension set up for that.)
