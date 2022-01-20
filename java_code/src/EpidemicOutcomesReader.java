import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Scanner;

/**
 * Created by rebecca on 07/02/2017.
 */
public class EpidemicOutcomesReader {

    // Delimiter used in CSV file
    private static final String COMMA_DELIMITER = ",";
    private static final String NEW_LINE_SEPARATOR = "\n";

    /**
     * Constructor for reading in epidemic outcomes
     * @param fEO
     * @param includeTicToc
     * @return
     */
    public static ArrayList<EpidemicOutcomes> readEpidemicOutcomes(String fEO, Boolean includeTicToc) {
        String paramID, runID;
        int initNCases, nDogsIntroducedCell;
        double startInDays;
        int nDemographicEvents = 0;
        int nBiteEvents = 0, nTransmissions = 0, nAbandonments = 0, nBitesE = 0, nBitesV = 0, nFailedTransmissions = 0, nReinfections = 0;
        int nDistinctS = 0, nDistinctE = 0, nDistinctV = 0;
        int nDiedE = 0;
        double endInDays, lastCase;
        int nICases, nERemaining;
        String initialMixedPop, finalMixedPop;
        double tic = 0, toc = 0, runTime = 0; // Set to zero in case not used
        String stoppingReason;
        ArrayList<Case> caseList;         // Final caseList for output

        ArrayList<EpidemicOutcomes> eo = new ArrayList<EpidemicOutcomes>();

        if (fEO.length()>0) {

            final String CSV_DELIMITER = ", *|\\n|\\n\\r";      // delimiters in csv file are new lines and commas + arbitrary num of spaces
            // Read in list of initial incubating cases
            Scanner sc = null;

            // Initialise scanner for reading in
            try {
                sc = new Scanner(new File(fEO));
                sc.useDelimiter(NEW_LINE_SEPARATOR);   // delimiters in csv file are new lines and commas + arbitrary num of spaces
                String header = sc.next();

                // Now read in items separated by commas
                sc.useDelimiter(CSV_DELIMITER);   // delimiters in csv file are new lines and commas + arbitrary num of spaces

                while (sc.hasNext()) {

                    // Columns in csv
                    paramID = sc.next();
                    runID = sc.next();
                    initNCases = Integer.parseInt(sc.next());
                    nDogsIntroducedCell = Integer.parseInt(sc.next());
                    startInDays = Double.parseDouble(sc.next());
                    nDemographicEvents = Integer.parseInt(sc.next());
                    nBiteEvents = Integer.parseInt(sc.next());
                    nTransmissions = Integer.parseInt(sc.next());
                    nAbandonments = Integer.parseInt(sc.next());
                    nBitesE = Integer.parseInt(sc.next());
                    nBitesV = Integer.parseInt(sc.next());
                    nFailedTransmissions = Integer.parseInt(sc.next());
                    nReinfections = Integer.parseInt(sc.next());
                    nDistinctS = Integer.parseInt(sc.next());
                    nDistinctE = Integer.parseInt(sc.next());
                    nDistinctV = Integer.parseInt(sc.next());
                    nDiedE = Integer.parseInt(sc.next());
                    endInDays = Double.parseDouble(sc.next());
                    lastCase = Double.parseDouble(sc.next());
                    stoppingReason = sc.next();
                    nICases = Integer.parseInt(sc.next());
                    nERemaining = Integer.parseInt(sc.next());
                    initialMixedPop = sc.next();
                    finalMixedPop = sc.next();
                    if (includeTicToc) {
                        tic = Double.parseDouble(sc.next());
                        toc = Double.parseDouble(sc.next());
                        runTime = Double.parseDouble(sc.next());
                    }

                    // Throw away any extra info on this line
                    if (sc.hasNext()) {
                        sc.useDelimiter(NEW_LINE_SEPARATOR);
                        sc.next();
                        sc.useDelimiter(CSV_DELIMITER);
                    }

                    // Add to the list of epidemic.outcomes
                    eo.add(new EpidemicOutcomes(paramID,initNCases,nDogsIntroducedCell,startInDays,nDemographicEvents,nBiteEvents,nTransmissions,
                            nAbandonments, nBitesE, nBitesV, nFailedTransmissions,nReinfections,nDistinctS, nDistinctE, nDistinctV,nDiedE, endInDays, lastCase, stoppingReason,
                            nICases, nERemaining, initialMixedPop, finalMixedPop, tic, toc, runTime));
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
        return(eo);
    }

}
