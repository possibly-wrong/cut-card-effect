// Compute all possible completed blackjack rounds (player hands only)

#include "blackjack.h"
#include <cstdint>
#include <set>
#include <vector>
#include <iostream>

// A game state records a decision point during a round:
//   round[0] = # hands in the round
//   round[1:] = # cards of each rank dealt in the round (excluding up card)
//   hand[0] = index of current hand in the round
//   hand[1:] = # cards of each rank dealt in the current hand
//   pair_card = 1-10 if split, otherwise 0
//   stand = true iff at least one hand is not busted (dealer must resolve)
struct State
{
    std::int8_t round[11];
    std::int8_t hand[11];
    std::int8_t pair_card;
    std::int8_t stand;

    // Comparator for std::set
    bool operator<(const State& rhs) const
    {
        for (int card = 0; card < 11; ++card)
        {
            if (round[card] != rhs.round[card])
            {
                return (round[card] < rhs.round[card]);
            }
            if (hand[card] != rhs.hand[card])
            {
                return (hand[card] < rhs.hand[card]);
            }
        }
        if (pair_card != rhs.pair_card)
        {
            return (pair_card < rhs.pair_card);
        }
        return (stand < rhs.stand);
    }
};

struct Game
{
    // Define rule variations:
    Game(const BJShoe& shoe,
        bool h17, bool das, bool doa, bool d9, int spl, bool rsa) :
        shoe{shoe},
        H17{h17},   // dealer hits soft 17
        DAS{das},   // double down after split
        DOA{doa},   // double down on any two cards (only valid if DAS)
        D9{d9},     // double down on 9 (only valid if !DOA)
        SPL{spl},   // maximum number of splits
        RSA{rsa},    // resplit aces
        rounds{}
    {
        // empty
    }

    // Play out all possible completed rounds vs. dealer up card using given
    // playing strategy, storing results in rounds[:].
    void play(int up_card, BJStrategy& strategy)
    {
        rounds[0].clear();
        rounds[1].clear();
        shoe.deal(up_card);
        std::set<State> level;
        State s0{};
        s0.round[0] = 1;
        level.insert(s0);
        while (!level.empty())
        {
            std::set<State> next;
            for (const auto& s : level)
            {
                if (s.hand[0] == s.round[0])
                {
                    std::vector<std::int8_t> round{s.round + 1, s.round + 11};
                    rounds[s.stand].insert(round);
                    continue;
                }
                BJHand hand{};
                int pair_card = 0;
                for (int card = 1; card <= 10; ++card)
                {
                    for (int i = 0; i < s.hand[card]; ++i)
                    {
                        hand.deal(card);
                        pair_card = card;
                    }
                }
                int option = BJ_HIT;
                if (hand.getCards() == 2 && s.pair_card == 1)
                {
                    option = BJ_STAND;
                }
                else if (hand.getCards() >= 2)
                {
                    bool double_allowed = (hand.getCards() == 2 &&
                        (DOA || hand.getCount() == 10 ||
                            hand.getCount() == 11 ||
                        (D9 && hand.getCount() == 9)) &&
                            (s.pair_card == 0 || DAS));
                    bool split_allowed = (hand.getCards() == 2 &&
                        hand.getCards(pair_card) == 2 && s.round[0] <= SPL &&
                        (RSA || s.pair_card != 1));
                    option = strategy.getOption(hand, up_card,
                        double_allowed, split_allowed, false);
                }
                switch (option)
                {
                case BJ_STAND:
                    next.insert(stand(s, hand));
                    break;
                case BJ_HIT:
                    hit(next, s, hand, false);
                    break;
                case BJ_DOUBLE_DOWN:
                    hit(next, s, hand, true);
                    break;
                case BJ_SPLIT:
                    next.insert(split(s));
                    break;
                default:
                    std::cerr << "ERROR!" << std::endl;
                }
            }
            level = next;
        }
        shoe.undeal(up_card);
    }

    // Return new state from standing on current hand.
    State stand(State s, const BJHand& hand)
    {
        ++s.hand[0];
        for (int card = 1; card <= 10; ++card)
        {
            s.hand[card] = 0;
        }
        if (s.pair_card != 0)
        {
            s.hand[s.pair_card] = 1;
        }
        if (hand.getCount() <= 21)
        {
            s.stand = 1; // need to play dealer's hand
        }
        return s;
    }

    // Extend list of next states from drawing to current hand.
    void hit(std::set<State>& moves, const State& s, BJHand& hand,
        bool doubled = false)
    {
        for (int card = 1; card <= 10; ++card)
        {
            if (s.round[card] < shoe.getCards(card))
            {
                State next{s};
                ++next.round[card];
                ++next.hand[card];
                hand.deal(card);
                moves.insert(doubled || hand.getCount() > 21 ?
                    stand(next, hand) : next);
                hand.undeal(card);
            }
        }
    }

    // Return new state from splitting current pair hand.
    State split(State s)
    {
        ++s.round[0];
        for (int card = 1; card <= 10; ++card)
        {
            if (s.hand[card] == 2)
            {
                s.pair_card = card;
                s.hand[card] = 1;
                break;
            }
        }
        return s;
    }

    BJShoe shoe;
    bool H17;
    bool DAS;
    bool DOA;
    bool D9;
    int SPL;
    bool RSA;
    std::set<std::vector<std::int8_t> > rounds[2];
};

int main()
{
    int num_decks = 6;
    std::cerr << "\nNumber of decks: ";
    std::cin >> num_decks;
    bool h17 = false;
    std::cerr << "\nDealer hits soft 17 (1=yes, 0=no): ";
    std::cin >> h17;
    bool das = true;
    std::cerr << "\nDouble down after split (1=yes, 0=no): ";
    std::cin >> das;
    bool doa = true;
    bool d9 = true;
    if (das)
    {
        std::cerr << "\nDouble down on any two cards (1=yes, 0=no): ";
        std::cin >> doa;
        if (!doa)
        {
            std::cerr << "\nDouble down on 9 (1=yes, 0=no): ";
            std::cin >> d9;
        }
    }
    int spl = 3;
    std::cerr << "\nMaximum number of splits (3=SPL3): ";
    std::cin >> spl;
    bool rsa = true;
    if (spl > 1)
    {
        std::cerr << "\nRe-split aces (1=yes, 0=no): ";
        std::cin >> rsa;
    }
    BJShoe shoe{num_decks};
    Game game{shoe, h17, das, doa, d9, spl, rsa};

    BJRules rules{h17, doa, d9, true, false, das, spl > 1, rsa, false};
    BJStrategy strategy{};
    BJProgress progress{};
    BJPlayer *player = new BJPlayer(shoe, rules, strategy, progress);
    for (int up_card = 1; up_card <= 10; ++up_card)
    {
        std::cout << up_card << std::endl;
        game.play(up_card, *player);
        for (int stand = 0; stand < 2; ++stand)
        {
            std::cout << game.rounds[stand].size() << std::endl;
            for (const auto& round : game.rounds[stand])
            {
                for (auto card : round)
                {
                    std::cout << int(card) << " ";
                }
                std::cout << std::endl;
            }
        }
    }
    delete player;
}
