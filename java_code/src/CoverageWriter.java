import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;

/**
 * Created by rebeccamancy on 11/09/2015.
 * Used to write out vaccination coverage
 */
public class CoverageWriter {
    // Delimiter used in CSV file
    private static final String COMMA_DELIMITER = ",";
    private static final String NEW_LINE_SEPARATOR = "\n";
    FileWriter fileWriter = null;

    // CSV file header
    private static final String FILE_HEADER = Population.populationCSVHeader();

    /**
     * Constructor
     * @param fileName
     */
    public CoverageWriter(String fileName) {

        if (fileName.length()>0) {

            try {
                fileWriter = new FileWriter(fileName);

                // Write the CSV file header and new line
                fileWriter.append(FILE_HEADER.toString());
                fileWriter.append(NEW_LINE_SEPARATOR);

            } catch (Exception e) {
                System.out.println("Error in CsvFileWriter !!!");
                e.printStackTrace();
            }
        }
    }

    /**
     * Appends information to coverage output file
     * @param mp
     * @param days
     */
    public void appendCsvFile(Metapopulation mp, double days) {

         try {

            // Write current populations to the CSV file
            for (Population p : mp.getPopulations()) {
                fileWriter.append(days + "," + p.toCSVString());
                fileWriter.append(NEW_LINE_SEPARATOR);
            }

        } catch (Exception e) {
            System.out.println("Error in CsvFileWriter !!!");
            e.printStackTrace();

        }
    }

    /**
     * Close CSV file containing vaccination coverage information
     */
    public void closeCSv() {
        try {
            fileWriter.flush();
            fileWriter.close();
        } catch (IOException e) {
            System.out.println("Error while flushing/closing fileWriter !!!");
            e.printStackTrace();
        }
    }
}
