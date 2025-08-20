# Memory Card Game

Test your memory skills in this blockchain-based card matching game and earn STX rewards based on your efficiency.

## Features

- 4x4 grid memory card game (16 cards, 8 pairs)
- Efficiency-based reward system
- On-chain game state and validation
- STX entry fee and rewards
- Performance tracking with move counting

## How to Play

1. Pay 0.5 STX entry fee to start a new game
2. Reveal cards by calling `reveal-card` with position (0-15)
3. Use `check-match` to verify if two revealed cards match
4. Complete all 8 pairs to finish the game
5. Claim your reward based on efficiency (fewer moves = higher reward)

## Smart Contract Functions

- `start-game`: Begin new memory game (0.5 STX fee)
- `reveal-card`: Reveal a card at specific position
- `check-match`: Check if two positions match
- `claim-reward`: Collect your efficiency-based reward
- `get-game`: View complete game state
- `get-card-at-position`: Check card value at position

## Reward System

- Base Reward: 1 STX for completion
- Efficiency Bonus: Additional STX based on moves used
- Formula: Base + (10 STX / moves_used)
- Perfect game (16 moves) = ~1.625 STX total reward