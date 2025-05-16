Needs["AuthorTools`"]
SetDirectory[NotebookFolder[EvaluationNotebook[]]];

decks = 6;
$hitSoft17 = True;
shoe = decks {4, 4, 4, 4, 4, 4, 4, 4, 4, 16};
pen = Floor[52 decks*0.75];

deal[{cards_, handCount_, handSoft_}, card_] :=
 Module[
  {newCards, newCount, newSoft},
  newCards = cards;
  newCards[[card]]++;
  newCount = handCount + card;
  newSoft = handSoft;
  If[card === 1 && newCount < 12,
   newCount += 10; newSoft = True
   ];
  If[newCount > 21 && newSoft,
   newCount -= 10; newSoft = False
   ];
  {newCards, newCount, newSoft}
  ]

resolveDealer[{cards_, handCount_, handSoft_}, upCard_] :=
 If[handCount < 17 || ($hitSoft17 && handCount === 17 && handSoft),
  Do[
   resolveDealer[deal[{cards, handCount, handSoft}, card], upCard],
   {card, 10}
   ],
  Sow[cards]
  ]
  
raw = ReadList["cut_card.txt", Number, RecordLists -> True];
player = Table[
   raw = Rest[raw];
   Table[
    {n, raw} = {First[raw], Rest[raw]};
    data = Take[raw, First[n]];
    raw = Drop[raw, First[n]];
    data,
    {stand, 2}
    ],
   {upCard, 10}
   ];

dealer = DeleteCases[
   Table[
    {
     Table[
      UnitVector[10, upCard] + UnitVector[10, card],
      {card, 10}
      ],
     resolveDealer[
          deal[{Table[0, {10}], 0, False}, upCard],
          upCard
          ] // Reap // Last // Last // Union
     },
    {upCard, 10}
    ],
   {1, 0, 0, 0, 0, 0, 0, 0, 0, 1}, {3}
   ];

rounds = Join[
    Join @@ Table[
      {1, 0, 0, 0, 0, 0, 0, 0, 0, 1} + UnitVector[10, card1] + 
       UnitVector[10, card2],
      {card1, 10}, {card2, 10}
      ],
    Flatten[MapThread[
      Union[Join @@ Outer[Plus, #1, #2, 1]] &,
      {player, dealer}, 2
      ], 2]
    ] // Union;

rounds = Extract[rounds,
   Position[
    Min /@ Transpose[shoe - Transpose[rounds]],
    _?NonNegative
    ]];
    
soln = LinearProgramming[
   ConstantArray[1, Length[rounds]],
   Append[-Transpose[rounds], Total /@ rounds],
   Append[-shoe, pen],
   Automatic, Integers
   ];
