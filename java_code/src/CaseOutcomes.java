import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Scanner;

/**
 * Created by rebeccamancy on 13/06/2016.
 */
public class CaseOutcomes {
    private int nTransmissions, nAbandonments, nBitesE, nBitesV, nFailedTransmissions, nReinfections;
    private int nDistinctS, nDistinctE, nDistinctV, nDistinctDogs; // These are the numbers of distinct dogs BITTEN
    private Boolean diedE;

    /**
     * Constructor used when dies in incubating (Exposed = E) stage
     */
    public CaseOutcomes(){
        this.nAbandonments = 0;
        this.nBitesE = 0;
        this.nBitesV = 0;
        this.nTransmissions = 0;
        this.nFailedTransmissions = 0;
        this.nReinfections = 0;
        this.diedE = Boolean.FALSE;
        this.nDistinctS = 0;
        this.nDistinctE = 0;
        this.nDistinctV = 0;
    }

    /**
     * Constructor used when reading in complete cases, with outcomes
     * @param nAbandonments
     * @param nBitesE
     * @param nBitesV
     * @param nTransmissions
     * @param nFailedTransmissions
     * @param nReinfections
     * @param diedE
     * @param nDistinctDogs
     * @param nDistinctS
     * @param nDistinctE
     * @param nDistinctV
     */
    public CaseOutcomes(int nAbandonments, int nBitesE, int nBitesV, int nTransmissions, int nFailedTransmissions, int nReinfections, Boolean diedE,
                        int nDistinctDogs, int nDistinctS, int nDistinctE, int nDistinctV){
        this.nAbandonments = nAbandonments;
        this.nBitesE = nBitesE;
        this.nBitesV = nBitesV;
        this.nTransmissions = nTransmissions;
        this.nFailedTransmissions = nFailedTransmissions;
        this.nReinfections = nReinfections;
        this.diedE = diedE;
        this.nDistinctDogs = nDistinctDogs;
        this.nDistinctS = nDistinctS;
        this.nDistinctE = nDistinctE;
        this.nDistinctV = nDistinctV;
    }

    // Getters, setters and count increment methods
    public void setDiedE (Boolean diedE) {
        this.diedE = diedE;
    }
    public void incrementNAbandonments () { this.nAbandonments ++; }
    public void incrementNBitesE(int n) { this.nBitesE = this.nBitesE + n; }
    public void incrementNBitesV(int n) { this.nBitesV = this.nBitesV + n; }
    public void incrementNFailedTransmissions() { this.nFailedTransmissions ++; }
    public void incrementNTransmissions() { this.nTransmissions ++;}
    public void incrementNReinfections() { this.nReinfections ++; }
    public void incrementNDistinctS(int n) { this.nDistinctS = nDistinctS + n; }
    public void incrementNDistinctE(int n) { this.nDistinctE = nDistinctE + n; }
    public void incrementNDistinctV(int n) { this.nDistinctV = nDistinctV + n; }
    public void incrementNDistinctDogs(int n) { this.nDistinctDogs = nDistinctDogs + n; }

    public int getnBiteEvents() { return getSummedEvents();}
    public int getnTransmissions() { return nTransmissions; }
    public int getnBitesE() { return nBitesE; }
    public int getnBitesV() {return nBitesV; }
    public int getnAbandonments() { return nAbandonments; }
    public int getnFailedTransmissions() { return nFailedTransmissions; }
    public int getnReinfections() { return nReinfections; }
    public Boolean getDiedE() { return diedE; }
    public int getnDistinctS() { return nDistinctS; }
    public int getnDistinctE() { return nDistinctE; }
    public int getnDistinctV() { return nDistinctV; }
    public int getnDistinctDogs() { return nDistinctDogs; }
    public int getSummedEvents() {
        int sum = nAbandonments + nBitesE + nBitesV + nTransmissions + nFailedTransmissions + nReinfections;
        return sum;
    }

    /**
     * Converts CaseOutcomes to string
     * @return
     */
    public String toString() {
        return "nBiteEvents:" + getSummedEvents() + " nAbandonments:" + nAbandonments + " nBitesE:" + nBitesE + " nBitesV:" + nBitesV +
                " nTransmissions:" + nTransmissions + " nFailedTransmissions:" + nFailedTransmissions + " nReinfections:" + nReinfections +
                " nDistinctS:" + nDistinctS + " nDistinctE:" + nDistinctE + " nDistinctV:" + nDistinctV + " nDistinctDogs:" + getnDistinctDogs();
    }

}
