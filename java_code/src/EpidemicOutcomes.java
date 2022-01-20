
import java.lang.reflect.Field;
import java.util.ArrayList;


/**
 * Created by rebeccamancy on 15/06/2016.
 */
public class EpidemicOutcomes {
    private String paramID, runID;
    private int initNCases, nDogsIntroducedCell;
    double startInDays;
    private int nDemographicEvents = 0;
    private int nBiteEvents = 0, nTransmissions = 0, nAbandonments = 0, nBitesE = 0, nBitesV = 0, nFailedTransmissions = 0, nReinfections = 0;
    private int nDistinctS = 0, nDistinctE = 0, nDistinctV = 0;
    private int nDiedE = 0;
    private double endInDays, lastCase;
    private String stoppingReason;
    int nICases, nERemaining;
    private Population initialMixedPop, finalMixedPop;
    private double tic, toc, runTime;
    private ArrayList<Case> caseList;         // Final caseList for output

    public String getParamID() {
        return paramID;
    }
    public double getEndInDays() {
        return endInDays;
    }

    public EpidemicOutcomes() { }

    public EpidemicOutcomes(String paramID, int initNCases, int nDogsIntroducedCell, double startInDays, int nDemographicEvents,
                           int nBiteEvents, int nTransmissions, int nAbandonments, int nBitesE, int nBitesV, int nFailedTransmissions, int nReinfections,
                           int nDistinctS, int nDistinctE, int nDistinctV, int nDiedE,
                           double endInDays, double lastCase,
                           String stoppingReason,
                           int nICases, int nERemaining,
                           String initialMixedPop, String finalMixedPop,
                           double tic, double toc, double runTime) {

        this.paramID = paramID;
        this.initNCases = initNCases;
        this.nDogsIntroducedCell = nDogsIntroducedCell;
        this.startInDays = startInDays;
        this.nDemographicEvents = nDemographicEvents;
        this.nBiteEvents = nBiteEvents;
        this.nTransmissions = nTransmissions;
        this.nAbandonments = nAbandonments;
        this.nBitesE = nBitesE;
        this.nBitesV = nBitesE;
        this.nFailedTransmissions = nFailedTransmissions;
        this.nReinfections = nReinfections;
        this.nDistinctS = nDistinctS;
        this.nDistinctE = nDistinctE;
        this.nDistinctV = nDistinctV;
        this.nDiedE = nDiedE;
        this.endInDays = endInDays;
        this.lastCase = lastCase;
        this.stoppingReason = stoppingReason;
        this.nICases = nICases;
        this.nERemaining = nERemaining;
        //this.initialMixedPop, finalMixedPop;
        this.tic = tic;
        this.toc = toc;
        this.runTime = runTime;
    }

    public void initialise(String paramID, double startInDays, int initNCases, int nDogsIntroducedCell, Population mxp, double tic) {
        this.paramID = paramID;
        this.startInDays = startInDays;
        this.initNCases = initNCases;
        this.nDogsIntroducedCell = nDogsIntroducedCell;
        this.initialMixedPop = new Population(mxp);
        this.tic = tic;
        caseList = new ArrayList<Case>();
    }

    public Population getInitialMixedPop() { return initialMixedPop; }
    public Population getFinalMixedPop() { return finalMixedPop; }

    public void incrementNDemographicEvents() {
        nDemographicEvents++;
    }

    private void incrementNBiteEvents(int n) {
        nBiteEvents = nBiteEvents + n;
    }

    private void incrementNTransmissions(int n) {
        nTransmissions = nTransmissions + n;
    }

    private void incrementNReinfections(int n) {
        nReinfections = nReinfections + n;
    }

    private void incrementNAbandonments(int n) {
        nAbandonments = nAbandonments + n;
    }

    private void incrementNBitesE(int n) {
        nBitesE = nBitesE + n;
    }

    private void incrementNBitesV(int n) {
        nBitesV = nBitesV + n;
    }

    private void incrementNFailedTransmissions(int n) {
        nFailedTransmissions = nFailedTransmissions + n;
    }

    public void incrementNDiedE() {
        nDiedE++;
    }

    private void incrementNDistinctS(int n) {
        nDistinctS = nDistinctS + n;
    }

    private void incrementNDistinctE(int n) {
        nDistinctE = nDistinctE + n;
    }

    private void incrementNDistinctV(int n) {
        nDistinctV = nDistinctV + n;
    }

    public ArrayList<Case> getCaseList() {
        return caseList;
    }

    public double getRunTime() {
        return runTime;
    }

    public String getStoppingReason() {return stoppingReason;}

    /**
     * Updates the EpidemicOutcomes by adding information from a new CaseOutcomes object
     *
     * @param co
     */
    public void update(CaseOutcomes co) {
        if (co.getDiedE()) {
            incrementNDiedE();
        } else {
            incrementNBiteEvents(co.getnBiteEvents());
            incrementNTransmissions(co.getnTransmissions());
            incrementNAbandonments(co.getnAbandonments());
            incrementNBitesE(co.getnBitesE());
            incrementNBitesV(co.getnBitesV());
            incrementNFailedTransmissions(co.getnFailedTransmissions());
            incrementNReinfections(co.getnReinfections());
            incrementNDistinctS(co.getnDistinctS());
            incrementNDistinctE(co.getnDistinctE());
            incrementNDistinctV(co.getnDistinctV());
        }
    }

    public void finalise(double endInDays, double lastCase, int nICases, int nERemaining, Population mxp, double toc, String stoppingReason) {
        final double millisecs2secs = 1000000000.0;
        this.endInDays = endInDays;
        this.lastCase = lastCase;
        this.nICases = nICases;
        this.nERemaining = nERemaining;
        this.finalMixedPop = mxp;
        this.toc = toc;
        this.runTime = (toc - tic) / millisecs2secs;
        this.stoppingReason = stoppingReason;
    }

    public String getHeader(Boolean includeTicToc) {
        final String CSV_SEPARATOR = ",";
        StringBuilder sb = new StringBuilder();
            try {
                // Write field names (header) if required
                for (Field f : this.getClass().getDeclaredFields()) {
                    if (! f.getName().equals("caseList")) {
                        if (includeTicToc || (!f.getName().equals("tic") && !f.getName().equals("toc") && !f.getName().equals("runTime"))) {
                            sb.append(f.getName());
                            sb.append(CSV_SEPARATOR);
                        }
                    }
                }
            } catch (Exception e) {
                System.err.println(e.toString());
            }
        return sb.toString();
    }

    public String getValues(Boolean includeTicToc) {
        final String CSV_SEPARATOR = ",";
        StringBuilder sb = new StringBuilder();
        try {
            // Write field names (header) if required
            for (Field f : this.getClass().getDeclaredFields()) {
                if (! f.getName().equals("caseList")) {
                    if (includeTicToc || (!f.getName().equals("tic") && !f.getName().equals("toc") && !f.getName().equals("runTime"))) {
                        sb.append(f.get(this));
                        sb.append(CSV_SEPARATOR);
                    }
                }
            }
        } catch (Exception e) {
            System.err.println(e.toString());
        }
        return sb.toString();
    }

}
