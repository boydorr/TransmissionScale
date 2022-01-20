/**
 * Created by rebeccamancy on 30/08/2015.
 * No longer used because we now read priors in from a file; maintained for backward compatibility
 */

import java.io.File;
import java.io.FileNotFoundException;
import java.util.HashMap;
import java.util.Scanner;

public class PriorsReader {

    // Delimiter used in CSV file
    private static final String COMMA_DELIMITER = ",";
    private static final String NEW_LINE_SEPARATOR = "\n";
    private static final String CSV_DELIMITER = ", *|\\n|\\n\\r";      // delimiters in csv file are new lines and commas + arbitrary num of spaces

    /**
     * Reads in priors from a csv file
     * @param inputFolder Folder
     * @param fPriors Filename of csv
     * @return HashMap containing priors
     */
    public static HashMap<String, Priors> readCSVFile(String inputFolder, String fPriors) {

        HashMap<String, Priors> priorsHashMap = new HashMap<String, Priors>();

        if (fPriors.length()>0) {

            // Read in from csv
            Scanner sc = null;
            String model, spatialScale;
            double ThMeanMin, ThMeanMax, ThSDMin, ThSDMax, TdMeanMin, TdMeanMax, incursionRateMin, incursionRateMax;

            // Initialise scanner for reading in
            try {
                System.out.println("Reading in priors from: " + inputFolder + fPriors);
                sc = new Scanner(new File(inputFolder + fPriors));
                sc.useDelimiter(NEW_LINE_SEPARATOR);   // delimiters in csv file are new lines and commas + arbitrary num of spaces
                String header = sc.next();

                // Now read in items separated by commas
                sc.useDelimiter(CSV_DELIMITER);   // delimiters in csv file are new lines and commas + arbitrary num of spaces

                while (sc.hasNext()) {

                    // Columns in csv
                    model = sc.next();  System.out.println("model: " + model);
                    spatialScale = sc.next();  System.out.println("spatialScale: " + spatialScale);
                    ThMeanMin = Double.parseDouble(sc.next());
                    ThMeanMax = Double.parseDouble(sc.next());
                    ThSDMin = Double.parseDouble(sc.next());
                    ThSDMax = Double.parseDouble(sc.next());
                    TdMeanMin = Double.parseDouble(sc.next());
                    TdMeanMax = Double.parseDouble(sc.next());
                    incursionRateMin = Double.parseDouble(sc.next());
                    incursionRateMax = Double.parseDouble(sc.next());

                    // Throw away any extra info on this line
                    if (sc.hasNext()) {
                        sc.useDelimiter(NEW_LINE_SEPARATOR);
                        sc.next();
                        sc.useDelimiter(CSV_DELIMITER);
                    }
                    // Add to the HashMap
                    priorsHashMap.put(model+"_"+spatialScale, new Priors(model, spatialScale, ThMeanMin, ThMeanMax, ThSDMin, ThSDMax, TdMeanMin, TdMeanMax, incursionRateMin, incursionRateMax ));
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
        return(priorsHashMap);
    }

}


