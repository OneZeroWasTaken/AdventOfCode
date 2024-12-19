
class Day15Part2
{
    public static void Solve()
    {
        var input = File.ReadAllLines("inputs/input15.txt");
        var (grid, dirs) = Parse(input);

        foreach (var dir in dirs)
        {
            Move(grid, GridFind(grid), dir);
        }

        int sum = grid.SelectMany((row, y) => row.Select((c, x) => c == '[' ? x + y * 100 : 0)).Sum();
        Console.WriteLine("Part 2: " + sum);
    }

    private static void Move(List<List<char>> grid, (int, int) current, Dir dir)
    {
        var (x, y) = current;
        var (dx, dy) = GetDelta(dir);
        if (grid[y + dy][x + dx] == '.')
        {
            grid[y + dy][x + dx] = '@';
            grid[y][x] = '.';
            return;
        }
        if (grid[y + dy][x + dx] == '#')
        {
            return;
        }

        if (dx != 0)
        {   // Move boxes horizontally
            
            while (grid[y][x + dx] != '#')
            {
                x += dx;
                if (grid[y][x] == '.')
                {
                    grid[y].RemoveAt(x);
                    grid[y].Insert(current.Item1, '.');
                    return;
                }
            }
            return;
        }
        if (dy != 0)
        {   // Move boxes vertically
            var positions = MovePositions(grid, (x, y + dy), dy);

            if (positions == null) return;

            foreach (var (cx, cy, from, to) in positions)
            {
                if (grid[cy][cx] == from)
                {
                    grid[cy][cx] = to;
                }
            }

            // Fix the empty space from the box positions that were moved
            var onlyPos = positions.Select(p => (p.Item1, p.Item2)).ToList();
            var filtered = onlyPos.Where(p => !onlyPos.Contains((p.Item1, p.Item2 - dy))).ToList();
            foreach (var (cx, cy) in filtered)
            {
                grid[cy][cx] = '.';
            }

            // Move @
            grid[y + dy][x] = '@';
            grid[y][x] = '.';
        }
    }

    private static List<(int, int, char, char)>? MovePositions(
        List<List<char>> grid,
        (int, int) current,
        int dy
    ) {
        var (x, y) = current;
        var positions = new List<(int, int, char, char)>();

        // If empty position, return
        if (grid[y][x] == '.')
        {
            positions.Add((x, y, '.', grid[y - dy][x]));
            return positions;
        }

        // If wall return null - cannot move
        if (grid[y][x] == '#')
        {
            return null;
        }
        
        // Else it is box, find move positions for both sides

        var (ox, oy) = grid[y][x] == ']' ? (x - 1, y) : (x + 1, y);

        List<(int, int, char, char)>? one;
        if (grid[y][x] == '.')
        {
            positions.Add((x, y, '.', grid[y - dy][x]));
            one = [];
        }
        else
        {
            one = MovePositions(grid, (x, y + dy), dy);
        }

        List<(int, int, char, char)>? two;
        if (grid[oy][ox] == '.')
        {
            positions.Add((ox, oy, '.', grid[oy - dy][ox]));
            two = [];
        }
        else
        {
            two = MovePositions(grid, (ox, oy + dy), dy);
        }

        if (one == null || two == null)
        {
            return null;
        }

        positions.Add((x, y, grid[y][x], grid[y - dy][x]));
        positions.Add((ox, oy, grid[oy][ox], grid[oy - dy][ox]));

        return [.. positions, .. one, .. two];
    }

    private static (int dx, int dy) GetDelta(Dir dir)
    {
        return dir switch
        {
            Dir.N => (0, -1),
            Dir.E => (1, 0),
            Dir.S => (0, 1),
            Dir.W => (-1, 0),
            _ => throw new NotImplementedException(),
        };
    }

    private static (int, int) GridFind(List<List<char>> grid)
    {
        for (int y = 0; y < grid.Count; y++)
        {
            for (int x = 0; x < grid[y].Count; x++)
            {
                if (grid[y][x] == '@')
                {
                    return (x, y);
                }
            }
        }
        throw new Exception("Not found");
    }

    private static void PrintGrid(List<List<char>> grid)
    {
        foreach (var l in grid)
        {
            Console.WriteLine(string.Join("", l));
        }
    }

    private static (List<List<char>>, List<Dir>) Parse(string[] input)
    {
        var map = new List<List<char>>();
        int i = 0;
        for (; i < input.Length; i++)
        {
            map.Add([.. input[i].Select(static x => Expand(x)).SelectMany(s => s)]);
            if (input[i] == "")
            {
                i++;
                break;
            }
        }
        var dir = new List<Dir>();
        for (; i < input.Length; i++)
        {
            dir.AddRange(input[i].Select(CharToDir).ToArray());
        }
        return (map, dir);
    }

    private static string Expand(char c)
    {
        return c switch
        {
            '#' => "##",
            '.' => "..",
            'O' => "[]",
            '@' => "@.",
            _ => throw new Exception("Unknown character")
        };
    }

    public static Dir CharToDir(char c)
    {
        return c switch
        {
            '^' => Dir.N,
            '>' => Dir.E,
            'v' => Dir.S,
            '<' => Dir.W,
            _ => throw new Exception("Unknown direction")
        };
    }
}

enum Dir
{
    N, E, S, W
}
