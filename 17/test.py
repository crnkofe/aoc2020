def day17_1():
    from collections import defaultdict
 
    with open("input.txt", "r") as file:
        data = file.readlines()
 
    grid = defaultdict(lambda: ".")
    for row in range(len(data)):
        for col in range(len(data[0]) - 1):
            grid[row, col, 0] = data[row][col]
    for key in grid:
        print(key, grid[key])
 
    def num_neighbors(coord):
        n = 0
        for i_1 in range(-1, 2):
            for i_2 in range(-1, 2):
                for i_3 in range(-1, 2):
                    if i_1 == i_2 == i_3 == 0:
                        continue
                    n += grid[coord[0] + i_1, coord[1] + i_2, coord[2] + i_3] == "#"
 
        return n
 
    for _ in range(2):
        print("round")
        new_grid = defaultdict(lambda: ".")
 
        x_bound = y_bound = z_bound = [10 ** 100, -(10 ** 100)]
        for k in grid.keys():
            x_bound = [min(x_bound[0], k[0]), max(x_bound[1], k[0])]
            y_bound = [min(y_bound[0], k[1]), max(y_bound[1], k[1])]
            z_bound = [min(z_bound[0], k[2]), max(z_bound[1], k[2])]
 
        for x in range(x_bound[0] - 1, x_bound[1] + 2):
            for y in range(y_bound[0] - 1, y_bound[1] + 2):
                for z in range(z_bound[0] - 1, z_bound[1] + 2):
                    neighbors = num_neighbors((x, y, z))
 
                    print(x, y, z, neighbors)
                    if grid[x, y, z] == "#":
                        if neighbors == 2 or neighbors == 3:
                            new_grid[x, y, z] = "#"
                    else:
                        if neighbors == 3:
                            new_grid[x, y, z] = "#"
 
        grid = defaultdict(lambda: ".", {k: v for k, v in new_grid.items()})
        for key in grid:
            if key[2] != -1:
                continue
            print(key, grid[key])
        for key in grid:
            if key[2] != 0:
                continue
            print(key, grid[key])
        for key in grid:
            if key[2] != 1:
                continue
            print(key, grid[key])
        # print(grid)
 
    total = 0
    for v in grid.values():
        if v == "#":
            total += 1
 
    return total
print(day17_1())
