import numpy as np
import time

def count_neighbors(grid, x, y):
    count = 0
    for i in range(-1, 2):
        for j in range(-1, 2):
            if (i != 0 or j != 0) and grid[x + i, y + j]:
                count += 1
    return count

def update(grid):
    new_grid = grid.copy()
    for x in range(grid.shape[0]):
        for y in range(grid.shape[1]):
            n = count_neighbors(grid, x, y)
            if grid[x, y]:
                if n < 2 or n > 3:
                    new_grid[x, y] = 0
            else:
                if n == 3:
                    new_grid[x, y] = 1
    return new_grid

def display(grid):
    print("\n".join(" ".join("#" if c else "." for c in row) for row in grid))

def game_of_life():
    grid = np.array([[0, 1, 0],
                     [0, 0, 1],
                     [1, 1, 1]])

    while True:
        display(grid)
        time.sleep(0.1)
        grid = update(grid)

if __name__ == "__main__":
    game_of_life()
