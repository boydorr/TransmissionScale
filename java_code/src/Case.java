
import com.vividsolutions.jts.geom.*;
import org.apache.commons.collections4.multimap.ArrayListValuedHashMap;
import java.util.*;


/**
 * Created by rebeccamancy on 04/01/2015.
 * Contains information about each case in the simulation
 */

public class Case implements Comparable<Case> {
    private static final String COMMA_DELIMITER = ",";

    /**
     * Cases are "initial", "incursions", or endogenously generated
     */
    public enum caseType {
        INITIAL, INCURSION, ENDOGENOUS, OBSERVED
    }

    // Attributes
    private int id;
    private final int parentID;                     // id of the parent case
    private double serialInterval;                  // time in incubating phase
    private double dayGenerated, dayInfectious, infectiousPeriod;     // Number of days since start of simulation
    private Point position;                   // position in space
    private String popName;                         // Name of population where it took place
    private int dogCount, dogsE, dogsV; private double dogDensityK;       // dogCount is number of dogs at point when became infectious, K is the carrying capacity of that population
    private int popID;
    private int strainID;                           // Used to track strains, introduced cases, etc. (convention: negative strain numbers are exogenously generated)
    private caseType typeOfCase;                    // Keeps track of how this case was generated (endogenous, incursion)
    private ArrayList<MovementModel.Excursion> excursionArrayList;
    private CaseOutcomes outcomes;
    private BiteRecord biteRecord;
    private double Th, Td;                          // Handling time, discovery time (these are dog traits drawn from distributions)
    private Point parentPosition;
    private int mpDogCount, mpDogsE, mpDogsV; private double mpK; // Total dog population information at the time of becoming infectious
    private double maxDistanceToSecondary; // Only used in postprocessor, maximum distance to secondary cases
    private Boolean positionAjusted = Boolean.FALSE;

    // Getters and setters
    public void setOutcomes(CaseOutcomes co) {
        this.outcomes = co;
    }
    public double getDogDensityK() {
        return dogDensityK;
    }
    public void setDogDensityK(double density) { dogDensityK = density; }
    public void setInfectiousPeriod(double infectiousPeriod) {
        this.infectiousPeriod = infectiousPeriod;
    }
    public void setSerialInterval(double serialInterval) {
        this.serialInterval = serialInterval;
    }
    public void setId(int id) {
        this.id = id;
    }
    public double getDayInfectious() {
        return dayInfectious;
    }
    public int getId() {
        return id;
    }
    public int getParentID() { return parentID; }
    public double getDayGenerated() {
        return dayGenerated;
    }
    public void setDayInfectious(double dayInfectious) {
        this.dayInfectious = dayInfectious;
    }
    public int getPopID() {
        return popID;
    }
    public int getStrainID() {
        return this.strainID;
    }
    public void setStrainID(int strainID) {
        this.strainID = strainID;
    }
    public void setTypeOfCase(caseType typeOfCase) { this.typeOfCase = typeOfCase; }
    public caseType getTypeOfCase() {
        return this.typeOfCase;
    }
    public Point getPosition() {
        return position;
    }
    public void setPosition(Point position) { this.position = position;}
    public CaseOutcomes getOutcomes() {
        return outcomes;
    }
    public double getInfectiousPeriod() { return infectiousPeriod; }
    public double getTh() {
        return Th;
    }
    public double getTd() {
        return Td;
    }
    public void setPopID(int popID) {
        this.popID = popID;
    }
    public Boolean getPositionAjusted() { return positionAjusted; }
    public void setPositionAjusted(Boolean positionAjusted) { this.positionAjusted = positionAjusted; }

    /**
     * Constructor for the index case and cases read in initially (no longer used)
     * @param serialInterval
     * @param position
     * @param population
     */
    public Case(double serialInterval, Point position, Population population) {
        this.id = 1;            // Index case assumed so id = 1
        this.parentID = 0;
        this.dayGenerated = 0;
        this.serialInterval = serialInterval;
        this.dayInfectious = dayGenerated + serialInterval;
        this.position = position;
        this.popName = population.getName();
        this.popID = population.getPopID();
        this.outcomes = new CaseOutcomes();
    }

    /**
     * Constructor for initial cases when constructed within Java
     * @param dayGenerated
     * @param epiVariableSet
     * @param position
     * @param population
     * @param strainID
     * @param typeOfCase
     */
    public Case(double dayGenerated, EpiVariableSet epiVariableSet, Point position, Population population, int strainID, caseType typeOfCase) {
        this.id = strainID;
        this.parentID = 0;
        this.dayGenerated = dayGenerated;
        this.serialInterval = epiVariableSet.getSerialInterval();
        this.dayInfectious = dayGenerated + serialInterval;
        this.infectiousPeriod = epiVariableSet.getInfectiousPeriod();
        this.position = position;
        this.parentPosition = this.getPosition();
        this.popName = population.getName();
        this.popID = population.getPopID();
        this.strainID = strainID;
        this.typeOfCase = typeOfCase;
        this.outcomes = new CaseOutcomes();
    }

    /**
     * Simple constructor used when drawing incursions from cases in the data
     * @param strainID
     * @param dayInfectious
     * @param typeOfCase
     * @param position
     */
    public Case(int strainID, double dayInfectious, caseType typeOfCase, Point position) {
        this.id = strainID;
        this.parentID = 0;
        this.dayInfectious = dayInfectious;
        this.position = position;
        this.parentPosition = this.getPosition();
        this.strainID = strainID;
        this.typeOfCase = typeOfCase;
        this.outcomes = new CaseOutcomes();
    }

    /**
     * Constructor for exogenous cases read in when all information about these is complete
     * @param id
     * @param parentID
     * @param popID
     * @param popName
     * @param dayGenerated
     * @param serialInterval
     * @param dayInfectious
     * @param infectiousPeriod
     * @param x_coord
     * @param y_coord
     * @param strainID
     * @param typeOfCase
     */
    public Case(int id, int parentID, int popID, String popName, double dayGenerated, double serialInterval, double dayInfectious, double infectiousPeriod,
                double x_coord, double y_coord, int strainID, caseType typeOfCase) {
        GeometryFactory factory = new GeometryFactory();
        this.id = id;
        this.parentID = parentID;
        this.popID = popID;
        this.popName = popName;
        this.position = factory.createPoint(new Coordinate(x_coord, y_coord));
        this.parentPosition = this.getPosition();
        this.dayGenerated = dayGenerated;
        this.serialInterval = serialInterval;
        this.dayInfectious = dayInfectious;
        this.infectiousPeriod = infectiousPeriod;
        this.strainID = strainID;
        this.typeOfCase = typeOfCase;
        this.outcomes = new CaseOutcomes();
    }

    /**
     * Constructor used for new (daughter) cases beyond the index case
     *      * NOTE THAT id is set separately (because we generate the new case in CaseGenerator, which doesn't know
     *      * how many cases we currently have ...)
     * @param parent
     * @param position
     * @param population
     * @param epiVariableSet
     */
    private Case(Case parent, Point position, Population population, EpiVariableSet epiVariableSet) {
        this.parentID = parent.getId();
        this.parentPosition = parent.getPosition();
        this.dayGenerated = parent.getDayInfectious();
        this.serialInterval = epiVariableSet.getSerialInterval();
        this.infectiousPeriod = epiVariableSet.getInfectiousPeriod();
        this.dayInfectious = dayGenerated + this.serialInterval;
        this.position = position;
        this.popName = population.getName();
        this.popID = population.getPopID();
        this.typeOfCase = caseType.ENDOGENOUS;
        this.strainID = parent.getStrainID();      // Gets the same strain as the parent
        this.outcomes = new CaseOutcomes();
    }

    /**
     * Generates a new daughter case based on the parent case
     * @param position
     * @param population
     * @param epiVariableSet
     * @return
     */
    private Case generateDaughter(Point position, Population population, EpiVariableSet epiVariableSet) {
        return new Case(this, position, population, epiVariableSet);
    }

    /**
     * Converts case information to a String
     * @return
     */
    public String toString() {
        String returnString = "id:" + id + ", parentID:" + parentID +
                ", dayGenerated:" + dayGenerated + ", dayInfectious:" + dayInfectious
                + ", popID:" + popID + ", location:(" + position.getX() + "," + position.getY() + ")" +
                ", strainID:" + strainID + ", typeOfCase:" + typeOfCase + " outcomes:" + outcomes.toString();
        return returnString;
    }

    /**
     * Computes the maximum distance to secondary cases within the list
     * @return
     */
    public ArrayList<Double> computeDistancesToSecondary(ArrayList<Case> caseList) {
        ArrayList<Double> dists = new ArrayList<>();
        for (Case sc : caseList) {
            if (sc.getParentID() == this.getId()) {
                double distance = sc.getPosition().distance(this.getPosition());
                dists.add(distance);
            }
        }
        return (dists);
    }

    /**
     * Converts case information to a String in csv format
     * @return
     */
    public String toStringCsv() {
        String returnString = id + COMMA_DELIMITER +
                parentID + COMMA_DELIMITER +
                popID + COMMA_DELIMITER +
                popName + COMMA_DELIMITER +
                dogCount + COMMA_DELIMITER +
                dogsE + COMMA_DELIMITER +
                dogsV + COMMA_DELIMITER +
                Th + COMMA_DELIMITER +
                Td + COMMA_DELIMITER +
                dogDensityK + COMMA_DELIMITER +
                dayGenerated + COMMA_DELIMITER +
                serialInterval + COMMA_DELIMITER +
                dayInfectious + COMMA_DELIMITER +
                infectiousPeriod + COMMA_DELIMITER +
                position.getX() + COMMA_DELIMITER +
                position.getY() + COMMA_DELIMITER +
                strainID + COMMA_DELIMITER +
                typeOfCase + COMMA_DELIMITER +
                outcomes.getnBiteEvents() + COMMA_DELIMITER +
                outcomes.getnDistinctDogs() + COMMA_DELIMITER +
                outcomes.getDiedE() + COMMA_DELIMITER +
                outcomes.getnAbandonments() + COMMA_DELIMITER +
                outcomes.getnBitesE() + COMMA_DELIMITER +
                outcomes.getnBitesV() + COMMA_DELIMITER +
                outcomes.getnReinfections() + COMMA_DELIMITER +
                outcomes.getnFailedTransmissions() + COMMA_DELIMITER +
                outcomes.getnTransmissions() + COMMA_DELIMITER +
                mpDogCount + COMMA_DELIMITER +
                mpDogsE + COMMA_DELIMITER +
                mpDogsV + COMMA_DELIMITER +
                outcomes.getnDistinctS() + COMMA_DELIMITER +
                outcomes.getnDistinctE() + COMMA_DELIMITER +
                outcomes.getnDistinctV();

        return returnString;
    }

    public static String toStringCsvHeader() {
        return "caseID,parentID,popID,popName,dogCount,dogsE,dogsV,Th,Td,dogDensityK,dayGenerated,serialInterval,dayInfectious,infectiousPeriod,x_coord,y_coord," +
                "strainID,typeOfCase,nExcursions,nDistinctDogs," +
                "diedE,nAbandonments,nBitesE,nBitesV,nReinfections,nFailedTransmissions,nTransmissions,mpDogCount,mpDogsE,mpDogsV,nDistinctS,nDistinctE,nDistinctV";
    }

    /**
     * Used to compute sorting order for list of cases for the list of exposed cases Treeset (sort by dayInfectious, then id)
     * @param other
     * @return
     */
    public int compareTo(final Case other) {
        if (other == null) {
            return 1;
        }
        final int comp = Double.compare(this.dayInfectious, other.dayInfectious);
        if (comp == 0) {
            return this.id - other.id;
        } else {
            return comp;
        }
    }

    /**
     * Subclass to hold information about each dog that's bitten
     */
    private class BittenDog {
        private final int dogID;
        private Epidemic.epiStatus status;
        private final ArrayList<Point> locationsEncountered;

        private BittenDog(int dogID, Point location) {
            this.dogID = dogID;
            locationsEncountered = new ArrayList<Point>();
            locationsEncountered.add(location);
        }

        private void setStatus(Epidemic.epiStatus status) {
            this.status = status;
        }

        private void addLocation(Point location) {
            locationsEncountered.add(location);
        }

        private int getDogID() {
            return dogID;
        }

        private Epidemic.epiStatus getStatus() {
            return status;
        }

        private ArrayList<Point> getLocationsEncountered() {
            return locationsEncountered;
        }

        public String toString() {
            return dogID + " " + status + " nLocations=" + locationsEncountered.size();
        }
    }

    /**
     * Subclass to hold the bite record of each case
     */
    private class BiteRecord {
        private final ArrayListValuedHashMap<Integer, BittenDog> popID_bittenDog = new ArrayListValuedHashMap(); // Integer stores popID
        private int nPops, nDistinctDogs, nDistinctS, nDistinctE, nDistinctV;

        private BiteRecord() {
            nPops = 0;
            nDistinctDogs = 0;
        }

        public String toString() {
            return "nPops=" + this.nPops + ", nDistinctDogs=" + nDistinctDogs + ", nDistinctS=" + nDistinctS + ", nDistinctE=" + nDistinctE + ", nDistinctV=" + nDistinctV;
        }

        public ArrayListValuedHashMap<Integer, BittenDog> getFullBiteInfo() {
            return popID_bittenDog;
        }

        /**
         * Adds a bite when the population is not currently listed
         * @param popID
         * @param dogID
         * @param location
         */
        private void addBiteNewPopulation(int popID, int dogID, Point location) {
            popID_bittenDog.put(popID, new BittenDog(dogID, location));
            nPops++;
            nDistinctDogs++;
        }

        /**
         * Adds a bite to a population that is already in the hashmap
         * @param popID
         * @param dogID
         * @param location
         */
        private void addBiteToPopulation(int popID, int dogID, Point location) {

            // Loop over existing dogs - if dog is found, add location to this dog
            Collection<BittenDog> dogsInPop = popID_bittenDog.get(popID);
            Iterator dogsInPopIt = dogsInPop.iterator();
            Boolean found = Boolean.FALSE;
            while (dogsInPopIt.hasNext()) {
                BittenDog d = (BittenDog) dogsInPopIt.next();
                if (d.getDogID() == dogID) {
                    found = Boolean.TRUE;
                    d.addLocation(location);
                }
            }
            // If dog not found, add a new dog within this population
            if (!found) {
                popID_bittenDog.put(popID, new BittenDog(dogID, location));
                nDistinctDogs++;
            }
        }

        /**
         * Adds bite to the BiteRecord, adding new population where required
         * @param popID
         * @param dogID
         * @param location
         */
        private void addBite(int popID, int dogID, Point location) {
            if (popID_bittenDog.containsKey(popID)) {
                addBiteToPopulation(popID, dogID, location);
            } else {
                addBiteNewPopulation(popID, dogID, location);
            }
        }

        /**
         * Returns a list of dog ids bitten
         * @param popID
         * @return
         */
        private ArrayList<Integer> getDogIDs(int popID) {
            ArrayList<Integer> dogIDList = new ArrayList();
            List<BittenDog> test = popID_bittenDog.get(popID);
            for (BittenDog d : test) {
                dogIDList.add(d.getDogID());
            }
            return dogIDList;
        }

        /**
         * Adds status information (S, E, V) to each of the BittenDog entries and updates CaseOutcomes record
         * @param mp
         */
        private void addStatus(Metapopulation mp) {
            Iterator popIt = popID_bittenDog.keySet().iterator();
            Epidemic.epiStatus currStatus;
            while (popIt.hasNext()) { // Loop over (unique) populations
                int popID = (Integer) popIt.next();
                // Get all BittenDog entries for this population
                ArrayList<Integer> dogIDList = getDogIDs(popID);
                ArrayList<Epidemic.epiStatus> statuses = Util.randDogStatusList(mp.getPopulations().get(popID), dogIDList, Main.rgen);
                for (int index = 0; index < statuses.size(); index++) {
                    currStatus = statuses.get((index));
                    popID_bittenDog.get(popID).get(index).setStatus(statuses.get(index));
                    switch (currStatus) {
                        case S:
                            nDistinctS ++; break;
                        case E:
                            nDistinctE ++; break;
                        case V:
                            nDistinctV ++; break;
                        default:
                            System.err.println("Unknown dog status in Case>addStatus. Aborting."); System.exit(0);
                    }
                }
            }
        }

        private int getnPops() {return nPops; }
        private int getnDistinctDogs() { return nDistinctDogs; }
        private int getnDistinctS() { return nDistinctS; }
        private int getnDistinctE() { return nDistinctE; }
        private int getnDistinctV() { return nDistinctV; }

        private ArrayListValuedHashMap<Integer, BittenDog> getPopID_BittenDog() {
            return popID_bittenDog;
        }
    }

    /**
     * Executes infections following bite events, and computes summary of outcomes
     * @param em
     * @param mp
     * @param br
     * @return
     */
    private TreeSet<Case> executeInfectionsAndSummariseOutcomes(EpidemiologicalModel em, Metapopulation mp, BiteRecord br) {
        TreeSet<Case> secondaryCases = new TreeSet<Case>();

        // Loop over all the dogs that were bitten in popID_bittenDog
        Iterator popIt = br.getPopID_BittenDog().keySet().iterator();
        while (popIt.hasNext()) { // Loop over (unique) populations
            int popID = (Integer) popIt.next();
            // Get all BittenDog entries for this population
            ArrayList<Integer> dogIDList = br.getDogIDs(popID);
            List<BittenDog> dogArrayList = br.getPopID_BittenDog().get(popID);
            for (BittenDog d : dogArrayList) {
                //System.out.println("In executeInfectionsAndSummariseOutcomes: " + d);
                switch (d.getStatus()) {
                    case E:
                        //System.out.println("Biting E " + d.getLocationsEncountered().size() + " times");
                        outcomes.incrementNBitesE(d.getLocationsEncountered().size());
                        break;
                    case V:
                        //System.out.println("Biting V " + d.getLocationsEncountered().size() + " times");
                        outcomes.incrementNBitesV(d.getLocationsEncountered().size());
                        break;
                    case S:
                        //System.out.println("Biting S " + d.getLocationsEncountered().size() + " times");
                        int nTransmissionsDog = 0;
                        // NB: Counts all transmissions, not rebites if no transmission
                        for (Point loc : d.getLocationsEncountered()) {   // Loop over locations encountered
                            if (nTransmissionsDog == 0) {  // Haven't yet transmitted to this dog
                                if (em.isTransmissionEffective()) {
                                    // This is a first transmission so generate a new case at this location
                                    secondaryCases.add(this.generateDaughter(loc, mp.getPopulations().get(popID), em.randEpiVariableSet()));
                                    outcomes.incrementNTransmissions();
                                    nTransmissionsDog++;
                                } else {
                                    outcomes.incrementNFailedTransmissions();
                                }
                            } else { // Have previously infected this dog
                                if (em.isTransmissionEffective()) {
                                    // This is a subsequent transmission - no new case, but increment nInfections
                                    outcomes.incrementNReinfections();
                                } else {
                                    // Failed transmission stochastically, although have already transmitted to this dog
                                    outcomes.incrementNFailedTransmissions();
                                }
                            }
                        }
                        break;
                    default:
                        System.out.println("Something wrong in summariseBiteInformation");
                        System.exit(0);
                }

            }

        }
        // Set density at mother location
        this.dogCount = mp.getPopulations().get(this.getPopID()).getTotalDogs();
        this.dogsE = mp.getPopulations().get(this.getPopID()).getDogsE();
        this.dogsV = mp.getPopulations().get(this.getPopID()).getDogsV();
        this.dogDensityK = mp.getPopulations().get(this.getPopID()).getK();
        // Set global density information
        this.mpDogCount = mp.getMixedPop().getTotalDogs();
        this.mpDogsE = mp.getMixedPop().getDogsE();
        this.mpDogsV = mp.getMixedPop().getDogsV();
        this.mpK = mp.getMixedPop().getK();

        // System.out.println("caseOutcomes has just been updated: " + this.outcomes.toString());
        return secondaryCases;
    }

    /**
     * Compiles information on Excursion events for this case into a BiteInformation object
     * adding information to CaseOutcomes on the number of distinct dogs bitten
     * @param mp
     * @return
     */
    private BiteRecord excursion2BiteInformation(Metapopulation mp) {
        BiteRecord br = new BiteRecord();

        // Loop over all excursions stored for this case and make a list of bites
        for (MovementModel.Excursion e : excursionArrayList) {
            if (e.getDogID() != -1) {      // If a dog was actually found on this excursions, then dogID >= 0
                // If this population is already in the record
                if (br.getPopID_BittenDog().containsKey(e.getPop().getPopID())) {
                    br.addBite(e.getPop().getPopID(), e.getDogID(), e.getLocation());
                } else { // Add as new population
                    br.addBiteNewPopulation(e.getPop().getPopID(), e.getDogID(), e.getLocation());
                }
            } else { // increment the number of abandonments (no dog there or took gave up before found)
                outcomes.incrementNAbandonments();
            }
        }
        // Add the status of all dogs in biteRecord
        if (br.getnDistinctDogs() > 0) {
            br.addStatus(mp);
            // System.out.println("Bite record: " + br.getPopID_BittenDog().toString());
        }

        // Update CaseOutcomes
        outcomes.incrementNDistinctS(br.nDistinctS);
        outcomes.incrementNDistinctE(br.nDistinctE);
        outcomes.incrementNDistinctV(br.nDistinctV);
        outcomes.incrementNDistinctDogs(br.getnDistinctDogs());

        ///System.out.println("Bit " + br.getnDistinctDogs() + " distinct dogs in " + br.getnPops() + " populations");
        ///System.out.println("S:" + br.getnDistinctS() + " E:" + br.getnDistinctE() + " V:" + br.getnDistinctV());
        return br;
    }

    /**
     * Function that executes biting and running
     * @param mv
     * @param em
     * @param mp
     * @return
     */
    public TreeSet<Case> doRunningBiting(MovementModel mv, EpidemiologicalModel em, Metapopulation mp) {
        BiteRecord biteRecord = computeBiteRecord(mv, mp, em);

        // If an INCURSION and didn't make any bites, repeat until does
        while (this.getTypeOfCase() == caseType.INCURSION && biteRecord.nDistinctDogs == 0) {
            biteRecord = computeBiteRecord(mv, mp, em);
        }

        // Execute the infection process, updating the system, and summarise outcomes
        TreeSet<Case> secondaryCases = executeInfectionsAndSummariseOutcomes(em, mp, biteRecord);
        ///System.out.println(secondaryCases);
        return secondaryCases;
    }

    /**
     * Computes a record of bites made for a Case object
     * @param mv
     * @param mp
     * @param em
     * @return
     */
    public BiteRecord computeBiteRecord(MovementModel mv, Metapopulation mp, EpidemiologicalModel em) {

        if (mv.getAdditionalString().equals("WO_DOG_VARIATION")) {
            Th = mv.getFixedTh();
        } else {
            Th = mv.getRandTh();
        }
        Td = mv.getRandTd(Th);

        //System.out.println("Case > computeBiteRecord > Td: " + Td);
        //System.out.println("In Case > doRunningBiting and Th = " + Th + ", Td = " + Td + ", infectious period = " + this.getInfectiousPeriod());

        excursionArrayList = new ArrayList<MovementModel.Excursion>();
        double elapsedTime = 0;
        Point currLocation = this.getPosition();
        MovementModel.Excursion currExcursion;

        // Get the time for the infection to last
        infectiousPeriod = this.getInfectiousPeriod();

        // Generate excursions while still alive (keep running and biting)
        while (elapsedTime < infectiousPeriod && excursionArrayList.size() < 1500) {
            // Set up an excursion and add excursion to the ArrayList for this case (first one is different as possible to have long-distance movement)
            if (excursionArrayList.size() == 0) {
                currExcursion = mv.getNewExcursion(currLocation, elapsedTime, infectiousPeriod, Td, Th, Boolean.TRUE);
            } else {
                currExcursion = mv.getNewExcursion(currLocation, elapsedTime, infectiousPeriod, Td, Th, Boolean.FALSE);
            }
            // System.out.println("currentExcursion: " + currExcursion.toString());
            elapsedTime = elapsedTime + currExcursion.getDuration();
            excursionArrayList.add(currExcursion);
            currLocation = currExcursion.getLocation(); // Update location so we get a path
        }
        // System.exit(1);

        // Compile all the excursions into potential bites
        this.biteRecord = excursion2BiteInformation(mp);

        /// System.out.println(excursionArrayList.toString());
        /// System.out.println(this.biteRecord.toString());

        // Execute the infection process, updating the system, and summarise outcomes
        ///TreeSet<Case> secondaryCases = executeInfectionsAndSummariseOutcomes(em, mp, biteRecord);
        ///System.out.println(secondaryCases);

        return this.biteRecord;
    }

}
