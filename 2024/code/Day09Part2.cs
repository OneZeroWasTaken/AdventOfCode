
class Day09Part2 {
    public static void Solve() {
        var input = File.ReadAllLines("inputs/input09.txt")[0];

        var disk = ParseDisk(input);
        var reversed = disk.AsEnumerable().Reverse().ToList();

        for (int i = 0; i < reversed.Count; i++)
        {
            if (reversed[i] is FileDisk file)
            {
                var index = disk.IndexOf(file);
                for (int j = 0; j < index; j++)
                {
                    if (IfFits(file, disk[j]))
                    {
                        var space = disk[j] as Space;
                        
                        disk[index] = new Space(file.Length);

                        disk[j] = file;
                        var newSpace = new Space(space!.Length - file.Length);
                        disk.Insert(j + 1, newSpace);
                        MergeSpaces(disk);
                        break;
                    }
                }
            }
        }

        Console.WriteLine("Part 2: " + Checksum(disk));
    }

    private static void MergeSpaces(List<Disk> disk)
    {
        for (int i = 0; i < disk.Count - 1; )
        {
            if (disk[i] is Space space1 && disk[i + 1] is Space space2)
            {
                disk[i] = new Space(space1.Length + space2.Length);
                disk.RemoveAt(i + 1);
            }
            else
            {
                i++;
            }
        }
    }

    private static long Checksum(List<Disk> disk)
    {
        long sum = 0;
        var actualIndex = 0;
        for (int i = 0; i < disk.Count; )
        {
            if (disk[i] is FileDisk file)
            {
                if (file.Length == 0)
                {
                    i++;
                    continue;
                }
                sum += file.Index * actualIndex;
                disk[i] = file with { Length = file.Length - 1 };
                actualIndex++;
            } else if (disk[i] is Space space)
            {
                actualIndex += space.Length;
                i++;
            }
        }
        return sum;
    }

    private static bool IfFits(FileDisk file, Disk other)
    {
        return other is Space && file.Length <= other.Length;
    }

    private static List<Disk> ParseDisk(string input)
    {
        return input.Select(c => (int)char.GetNumericValue(c))
            .Select<int, Disk>((v, i) => i % 2 == 0 ? new FileDisk(v, i / 2) : new Space(v))
            .ToList();
    }

    private abstract record Disk(int Length);
    private record FileDisk(int Length, int Index) : Disk(Length);
    private record Space(int Length) : Disk(Length);
}
