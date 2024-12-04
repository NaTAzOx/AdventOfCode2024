using System;
using System.IO;

class AdventOfCodeDecemberTheSecond
{
    static void Main(string[] args)
    {
        string line;
        int counter = 0;
        int totalLines = 0;
        int processedLines = 0;

        try
        {
            using (StreamReader streamReader = new StreamReader("C:\\Users\\NaTAzOx\\Downloads\\input2.txt"))
            {
                while ((line = streamReader.ReadLine()) != null)
                {
                    totalLines++;

                    // Skip empty lines
                    if (string.IsNullOrWhiteSpace(line))
                    {
                        continue;
                    }

                    processedLines++;

                    string[] spliited = line.Split(' ');
                    int[] ints = Array.ConvertAll(spliited, int.Parse);

                    bool isSafe = true;
                    bool increased = false;
                    bool decreased = false;
                    bool secondChance = false;

                    for (int index = 1; index < ints.Length; index++)
                    {
                        int difference = ints[index] - ints[index - 1];

                        if (difference < 0 && difference > -4 && !increased)
                        {
                            decreased = true;
                        }
                        else if (difference > 0 && difference < 4 && !decreased)
                        {
                            increased = true;
                        }
                        else if (difference == 0)
                        {
                            if (secondChance)
                            {
                                isSafe = false;
                                break;
                            }
                            secondChance = true;
                        }
                        else if (!secondChance)
                        {
                            secondChance = true;

                            if (index - 2 >= 0)
                            {
                                difference = ints[index] - ints[index - 2];

                                if (difference < 0)
                                {
                                    decreased = true;
                                }
                                else if (difference > 0)
                                {
                                    increased = true;
                                }
                            }
                        }
                        else
                        {
                            isSafe = false;
                            break;
                        }
                    }

                    if (isSafe)
                    {
                        counter++;
                        Console.WriteLine($"Line {totalLines} is safe: {line}");
                    }
                }
                Console.WriteLine($"Total lines read: {totalLines}");
                Console.WriteLine($"Total lines processed: {processedLines}");
                Console.WriteLine($"Safe lines count: {counter}");
            }
        }
        catch (Exception ex)
        {
            Console.WriteLine(ex.StackTrace);
        }
    }
}
