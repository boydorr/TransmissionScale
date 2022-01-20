/**
 * Created by rebeccamancy on 30/08/2015.
 */

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Scanner;

public class SamplingEpiVariableReader {

    // Delimiter used in CSV file
    private static final String NEW_LINE_SEPARATOR = "\n";
    private static final String CSV_DELIMITER = ", *|\\n|\\n\\r";      // delimiters in csv file are new lines and commas + arbitrary num of spaces

    /**
     * Reads a csv file containing epidemiological parameters (serial interval and infectious period)
     * @param inputFolder Folder
     * @param fEpiVariables file
     * @return ArrayList containing serial intervals and infectious periods
     */
    public static ArrayList<EpiVariableSet> readCSVFile(String inputFolder, String fEpiVariables) {

        ArrayList<EpiVariableSet> samplingEpiParamsArrayList = new ArrayList<EpiVariableSet>();

        if (fEpiVariables.length() > 0) {

            // Read in from csv
            Scanner sc = null;

            // Initialise scanner for reading in
            try {
                System.out.println("Reading in epidemiological parameters from: " + inputFolder + fEpiVariables);
                sc = new Scanner(new File(inputFolder + fEpiVariables));
                sc.useDelimiter(NEW_LINE_SEPARATOR);   // delimiters in csv file are new lines and commas + arbitrary num of spaces
                String header = sc.next();

                // Now read in items separated by commas
                sc.useDelimiter(CSV_DELIMITER);   // delimiters in csv file are new lines and commas + arbitrary num of spaces

                while (sc.hasNext()) {
                    double serialInterval, infectiousPeriod;
                    // Columns in csv
                    serialInterval = Double.parseDouble(sc.next());
                    infectiousPeriod = Double.parseDouble(sc.next());

                    // Add to the ArrayList
                    samplingEpiParamsArrayList.add(new EpiVariableSet(serialInterval, infectiousPeriod));
                }

            } catch (FileNotFoundException ex) {
                ex.printStackTrace();
                System.exit(1);
            } finally {
                if (sc != null) {
                    try {
                        sc.close();
                    }
                    catch (Exception ex) {
                        ex.printStackTrace();
                        System.exit(1);
                    }
                }
            }
        }
        return(samplingEpiParamsArrayList);
    }

}


