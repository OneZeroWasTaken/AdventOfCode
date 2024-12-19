using System;

namespace AdventOfCode2024
{
    internal class Program
    {
        static void Main(string[] args)
        {
            var day = args.Length > 0 ? args[0] : Console.ReadLine();
            RunDay(day);
        }

        static void RunDay(string? day)
        {
            switch (day)
            {
                case "9":
                    Day09Part2.Solve();
                    break;
                case "15":
                    Day15Part2.Solve();
                    break;
                default:
                    Console.WriteLine("Day not implemented or unknown argument");
                    break;
            }
        }
    }
}
