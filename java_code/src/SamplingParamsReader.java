/**
 * Created by rebeccamancy on 30/08/2015.
 **/

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Scanner;

public class SamplingParamsReader {

    // Delimiter used in CSV file
    private static final String COMMA_DELIMITER = ",";
    private static final String NEW_LINE_SEPARATOR = "\n";
    private static final String CSV_DELIMITER = ", *|\\n|\\n\\r";      // delimiters in csv file are new lines and commas + arbitrary num of spaces

    /**
     * Reads in sets of ThShape, ThScale, Td parameters from a csv of priors
     * @param inputFolder folder
     * @param fPriors file
     * @return ArrayList of ParamSet objects (priors on ThShape, ThScale, Td)
     */
    public static ArrayList<ParamSet> readCSVFile(String inputFolder, String fPriors) {

        ArrayList<ParamSet> samplingParamsArrayList = new ArrayList<ParamSet>();

        if (fPriors.length() > 0) {

            // Read in from csv
            Scanner sc = null;

            // Initialise scanner for reading in
            try {
                System.out.println("Reading in priors from: " + inputFolder + fPriors);
                sc = new Scanner(new File(inputFolder + fPriors));
                sc.useDelimiter(NEW_LINE_SEPARATOR);   // delimiters in csv file are new lines and commas + arbitrary num of spaces
                String header = sc.next();
                if (!header.equals("ThShape,ThScale,Td")) { System.out.println("Header of priors should be ThShape,ThScale,Td - Try again! They actually appear to be " + header); System.exit(0); }

                // Now read in items separated by commas
                sc.useDelimiter(CSV_DELIMITER);   // delimiters in csv file are new lines and commas + arbitrary num of spaces

                while (sc.hasNext()) {
                    double ThShape, ThScale, Td, incursionRate;
                    // Columns in csv
                    ThShape = Double.parseDouble(sc.next());
                    ThScale = Double.parseDouble(sc.next());
                    Td = Double.parseDouble(sc.next());

                    // Add to the ArrayList
                    samplingParamsArrayList.add(new ParamSet( ThShape, ThScale, Td ));
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
        return(samplingParamsArrayList);
    }

}


