
import jdistlib.rng.RandomEngine;
import java.util.*;

/**
 * Created by rebeccamancy on 04/01/2015.
 */
public class Epidemic {

    /**
     * Epidemiological status of dogs (Susceptible, Exposed, Vaccinated)
     */
    public enum epiStatus {
        S, E, V
    }

    /**
     * Types of events that can occur
     */
    private enum EventCategory {
        DEMOGRAPHIC, INFECTION, VACCINATION, GROWTH
    }

    private Metapopulation mp;
    private int maxCases;    // Current number of cases seen and max num allowed
    private double startInDays, endInDays, simTime;
    private EpidemiologicalModel epiModel;
    private MovementModel mvModel;
    private DemographicModel demographicModel;

    private TreeSet<Case> exposedCases;       // Incubating cases
    private TreeSet<Event> queuedEvents;      // Contains first event from each stream in a queue (ordered by time and then by id if the times are the same)
    private TreeSet<PopCoverage> popCoverageTreeSet;   // Used for storing overall dog counts per state per day
    private TreeSet<Double> updateKDays; // Days on which K will be updated
    private Boolean allowEndogenousToTransmit; // Set to false for R0 calculations
    private String additionalString; // Used to e.g. indicate a NAIVE_POP run (i.e. where we don't actually implement any transmissions, to test effects of clustering)

    private CoverageWriter coverageWriter;
    private MixedPopCoverageWriter mixedPopCoverageWriter;
    private String fByPopCoverageOutput, fMixedPopCoverageOutput;
    private EpidemicOutcomes eo;
    private int maxEventID = 4;
    private RandomEngine random;

    private int nextID;
    final private int daysInYear = 365, daysInMonth = 30;
    final private String C = ","; // comma

    /**
     * Constructs a new epidemic object
     * @param mp Metapopulation object (consisting of all the Populations)
     * @param maxCases Maximum permitted cases before aborting
     * @param startInDays Number of days from which to start simulating (usually zero)
     * @param endInDays Number of days at which to stop simulation
     * @param epiModel Epidemiological model
     * @param mvModel Movement model
     * @param dm Demographic model
     * @param fInitialCases File name of csv containing initial cases
     * @param fIncursions File name of csv containing incursion information
     * @param fMixedPopCoverageOutput File name of csv to which mixed population coverage should be written (or empty string)
     * @param fByPopCoverageOutput
     * @param formattedParamID ParamID as String
     */
    public Epidemic(Metapopulation mp, int maxCases, double startInDays, double endInDays, EpidemiologicalModel epiModel, MovementModel mvModel, DemographicModel dm,
                    String inputFolder, String folderInitialCases, String fInitialCases, String fIncursions, Boolean allowEndogenousToTransmit, String additionalString,
                    String fByPopCoverageOutput, String fMixedPopCoverageOutput, String formattedParamID,
                    RandomEngine random) {
        this.mp = mp;
        this.maxCases = maxCases;
        this.startInDays = startInDays;
        this.endInDays = endInDays;
        this.simTime = startInDays;  // Current time in days of the simulation
        this.random = random;
        this.allowEndogenousToTransmit = allowEndogenousToTransmit;
        this.additionalString = additionalString;

        // Set the models and CaseGenerator
        this.epiModel = epiModel;
        this.mvModel = mvModel;
        this.demographicModel = dm;

        // Make a copy of the TreeSet that contains the days on which to update Population carrying capacities
        this.updateKDays = (TreeSet) mp.getUpdateKDays().clone();
        ///System.out.println("Epidemic > updateKDays:" + updateKDays.toString());

        // Initialise storage for cases during simulations
        exposedCases = new TreeSet<Case>();
        queuedEvents = new TreeSet<Event>();

        // Initialise cases (reads in from file, or if empty string provided, chooses a population randomly for a single incursion)
        System.out.println("Reading initial cases from: " + fInitialCases);
        this.initialiseCases(fInitialCases);
        System.out.println("Reading incursion cases from: "  + fIncursions);
        this.incursions(fIncursions);
        this.initialiseQueuedEvents(simTime); // Initialise event queue for the start of the simulation
        nextID = setCaseID();

        // Error checking to ensure that no population has a negative number of dogs
        for (Population p : mp.getPopulations()) {
            if (p.getTotalDogs() < 0) {
                System.err.println("Total dogs in population " + p.getPopID() + " was " + p.getTotalDogs() + ". Aborting");
                System.exit(0);
            }
        }

        // Initialise the epidemic outcomes for final output and any filewriters required for coverage output
        this.eo = new EpidemicOutcomes();
        this.eo.initialise(formattedParamID, startInDays, exposedCases.size(), mp.getPopulations().get(exposedCases.first().getPopID()).getTotalDogs(), mp.getMixedPop(), System.nanoTime());
        this.fMixedPopCoverageOutput = fMixedPopCoverageOutput;
        this.fByPopCoverageOutput = fByPopCoverageOutput;
        /// System.out.println("Epidemic > fByPopCoverageOutput: " + fByPopCoverageOutput);
        this.initialiseCoverageOutput(fMixedPopCoverageOutput, fByPopCoverageOutput);

        /// Loop over all populations and check that none have negative E
        /*for (Population p : mp.getPopulations()) {
            if (p.getDogsE() < 0) {
                System.out.println("Problem in initialisation of Epidemic: negative # of exposed in " + p.toString());
            }
        }*/

        ///System.out.println("Epidemic successfully initialised.");
    }

    /**
     * Runs an epidemic
     * @param testRun Boolean.TRUE if we want to stop after a few cases
     * @param stopIfExtinct Boolean.TRUE if we want to stop simulating when goes extinct
     * @return an EpidemicOutcomes object
     */
    public EpidemicOutcomes run(Boolean testRun, Boolean stopIfExtinct, String additionalString) {

        int maxConcurrentCases = 10000;    // Maximum number of CONCURRENT cases
        if (additionalString.equals("R0_MULTI") || additionalString.equals("R0_MULTI_LOCATION")) {
            maxConcurrentCases = 50000;
        }
        double daysLastCase = -1;

        //System.out.println(endInDays + " endInDays from Epidemic>Run" + " max concurrent cases " + maxConcurrentCases);
        //System.out.println("Number of exposed cases: " + exposedCases.size() );

        String stoppingReason = "Completed";
        while(simTime < endInDays) {

            if (!additionalString.equals("NAIVE_POP")) { // Except when running a NAIVE_POP type simulation

                // Exit loop if we've exceeded maxCases
                if (eo.getCaseList().size() > maxCases) {
                    stoppingReason = "Exceeded maxCases";
                    break;
                }

                // Also exit loop if we've exceeded half the number of allowed cases in half the time
                if (simTime < 0.5 * endInDays && eo.getCaseList().size() > 0.5 * maxCases) {
                    stoppingReason = "Exceeded half maxCases by midpoint";
                    break;
                }

                // Stop if have too many cases at one time
                if (exposedCases.size() > maxConcurrentCases) {
                    stoppingReason = "Exceeded maxConcurrentCases";
                    break;
                }

                // Quit after a given number of cases if running in test mode
                if (testRun && eo.getCaseList().size() > 10) {
                    System.out.println("Test mode, quitting after a few cases.");
                    System.exit(0);
                }
            }
            // Get next event in the queue and remove from queue
            Event ev = queuedEvents.pollFirst();
            maxEventID ++;

            // If have passed a daily / monthly / yearly marker point then output and update as appropriate
            if (passedMarker(simTime, ev.eventTime, 1)) { // Daily marker passed
                /*if (popCoverageTreeSet != null) {
                    popCoverageTreeSet.add(new PopCoverage(simTime, mp.getMixedPop()));
                }*/
                if (passedMarker(simTime, ev.eventTime, daysInMonth)) { // Monthly marker passed
                    //this.periodicResetRates();
                    this.outputCoverage(fByPopCoverageOutput);
                    //System.out.println("rounded time=" + Math.round(simTime));
                }
                if (passedMarker(simTime, ev.eventTime, daysInYear)) { // Yearly marker passed
                    ///System.out.println("rounded time=" + Math.round(simTime));
                }
            }

            // --------- CONDUCT EVENTS ---------
            // Move simulator time forward and execute event
            simTime = ev.eventTime;
            ev.execute();

            // Set last case time if we've gone extinct
            if (exposedCases.size() == 0 && daysLastCase == -1) {
                daysLastCase = simTime;
            }
        }
        ///System.out.println("eo.getCaseList().size()=" + eo.getCaseList().size() + " maxCases=" + maxCases +
        ///        " simTime=" + simTime + " endInDays=" + endInDays + " stopIfExtinct=" + stopIfExtinct +
        ///        " exposedCases.size()=" + exposedCases.size() + " maxConcurrentCases=" + maxConcurrentCases);
        ///System.out.println(exposedCases.toString());

        // ------------ FINAL OUTPUT ------------
        eo.finalise(simTime, daysLastCase, eo.getCaseList().size(), exposedCases.size(), mp.getMixedPop(), System.nanoTime(), stoppingReason);
        ///System.out.println("in Epidemic > run after eo.finalise: " + eo.getInitialMixedPop().toString() + " " + eo.getFinalMixedPop().toString());
        // Final coverage and summary writing, and close files that are added to periodically
        mixedPopCoverageWriter.writeCsvFile(fMixedPopCoverageOutput, popCoverageTreeSet);
        if (fByPopCoverageOutput.length()>0) coverageWriter.closeCSv();

        System.out.println("This epidemic took " + String.format("%.2f", eo.getRunTime()) + " seconds to run.");

        return eo;
    }

    /**
     * Set initial ID of the first endogenously generated case
     *
     * @return
     */
    private int setCaseID() {
        nextID = 0;
        for (Case ec : exposedCases) {
            if (ec.getId() > nextID) {
                nextID = ec.getId();
            }
        }
        nextID++;
        return (nextID);
    }

    /**
     * Initialises the event queue - including demographic, infection and vaccination events
     * Initialises event queue by adding first event of each type
     * - demographic event [Gillespie]
     * - new infection event [from exposedCases] - including initial cases, endogenously-generated cases & incursions
     * - vaccination event [pre-programmed]
     * - update to carrying capacities [pre-programmed|
     *
     * @param simTime initial simulator time
     */
    private void initialiseQueuedEvents(double simTime) {

        // Time of next demographic event : requires total number of dogs in whole metapopulation
        double demogTime = simTime + demographicModel.nextDemogTau(mp.getMixedPop());  // time until next demographic event across whole metapopulation
        queuedEvents.add(new Event(EventCategory.DEMOGRAPHIC, demogTime, 0));

        // Time of next infection event, from exposedCases
        double infectionTime = exposedCases.first().getDayInfectious();
        queuedEvents.add(new Event(EventCategory.INFECTION, infectionTime, 1));

        /// / Time of next vaccination event : requires vaccination information
        if (mp.getVaccinationCampaignTreeset().size() > 0) {
            double vaccTime = mp.getVaccinationCampaignTreeset().first().getDayVaccinations();
            queuedEvents.add(new Event(EventCategory.VACCINATION, vaccTime, 2));
        }

        // Time of first carrying capacity change event
        if (updateKDays.size() > 0) {
            double evTime = updateKDays.pollFirst();
            queuedEvents.add(new Event(EventCategory.GROWTH, evTime, 3));
        }

        //System.out.println(queuedEvents.toString());
    }

    /**
     * Event - subclass
     */
    private class Event implements Comparable<Event> {
        private EventCategory eventCategory;
        private double eventTime;
        private int id;

        /**
         * Constructor
         * @param eventCategory
         * @param eventTime
         * @param id
         */
        private Event(EventCategory eventCategory, double eventTime, int id) {
            this.eventCategory = eventCategory;
            this.eventTime = eventTime;
            this.id = id;
        }

        // Getters and setters
        private double getEventTime() {
            return eventTime;
        }
        private EventCategory getEventCategory() {
            return eventCategory;
        }
        public String toString() {
            return eventTime + ", " + eventCategory + ", " + id;
        }


        /**
         * Allows us to order Events (e.g. in a Treeset) by Time, and then by id if the times are the same
         * @param other Other event to compare to
         * @return
         */
        public int compareTo(final Event other) {

            if (other == null) {
                return 1;
            }
            final int comp = Double.compare(this.eventTime, other.eventTime);
            if (comp == 0) {
                return this.id - other.id;
            } else {
                return comp;
            }

        }


        /**
         * Identifies the event type and executes an event
         */
        public void execute() {
            /// System.out.println("Epidemic > execute: Event " + this.toString());
            switch (this.getEventCategory()) {
                case DEMOGRAPHIC:
                    // System.out.println("Demographic event: " + this.toString());
                    this.doDemographicEvent(simTime);
                    eo.incrementNDemographicEvents(); // Basic accountancy of number of events
                    break;
                case INFECTION:
                    ///System.out.println("Infection event: " + this.toString());
                    this.implementInfectionEvent();
                    break;
                case VACCINATION:
                    ///System.out.println(simTime + "Vaccination event" + this.toString());
                    this.implementVaccinationEvent();
                    break;
                case GROWTH:
                    this.implementGrowthEvent(simTime);
                    if (popCoverageTreeSet != null) {
                        /// System.out.println("time = " + simTime + " in Epidemic, just updated for growth: " + mp.getMixedPop().toString());
                        popCoverageTreeSet.add(new PopCoverage(simTime, mp.getMixedPop()));
                    }
                    break;
                default:
                    System.err.println("Unknown event type " + eventCategory + ". Aborting!");
                    System.exit(0);
            }

        }

        /**
         * Implements a vaccination event
         */
        private void implementVaccinationEvent() {
            // Get next vaccination campaign and the population where occurred
            VaccinationCampaign currVC = mp.getVaccinationCampaignTreeset().pollFirst();

            if (currVC.getPopID() > -1) { // Campaigns by population (i.e. gridcell)
                Population focalPop = mp.getPopulations().get(currVC.getPopID());
                // Implement vaccination in the demographic model (i.e. move some dogs to state V)
                demographicModel.doVaccinationEvent(focalPop, mp.getMixedPop(), currVC.getDoses());
                // System.out.println("Implemented a vaccination event by popID in pop " + focalPop.toString());
            } else { // Campaigns by spatial unit when this was an administrative unit (deprecated)
                System.err.println("Deprecated in Epidemic");
                System.exit(0);
            }

            // -------- Add new VACCINATION Event to queuedEvents ---------
            if (mp.getVaccinationCampaignTreeset().size() > 0) {
                Event e = new Event(EventCategory.VACCINATION, mp.getVaccinationCampaignTreeset().first().getDayVaccinations(), maxEventID);
                queuedEvents.add(e);
            }

        }

        /**
         * Implement a carrying capacity change event
         * @param simTime Current simulation time
         */
        public void implementGrowthEvent (double simTime) {

            // Update all population carrying capacities according to information read in, and update rates
            for (Population pop : mp.getPopulations()) {
                pop.setK(pop.getMapDaysK().get(simTime), demographicModel);
            }

            // Add a new carrying capacity change event to queuedEvents
            if (updateKDays.size() > 0) {
                queuedEvents.add(new Event(EventCategory.GROWTH, updateKDays.pollFirst(), maxEventID)); // pollFirst removes first elt
            }

            // Recompute mixed population
            mp.combinePopsAgain();
        }

        /**
         * Execute a demographic event, and add a new next demographic event to the event queue
         * @param simTime Current simulation time used for generating a time for the next demographic event
         */
        public void doDemographicEvent(double simTime) {
            // --- DEMOGRAPHY PER SE - Select a population proportional to each population's rate, and action a demographic event there
            Population focalPop = demographicModel.propRatePop(mp);
            if(focalPop.getK()==0.0 && focalPop.getTotalDogs()>0) { // Error checking
                System.err.println("In Epi>doDemog: Error in pop " + focalPop.getPopID() + " K=" + focalPop.getK() + " N=" + focalPop.getTotalDogs() + " r=" + focalPop.getEventRate());
                System.exit(0);
            }
            // Update the population, returning TRUE if an exposed case died
            Boolean deathOfExposed = demographicModel.doEvent(focalPop, mp.getMixedPop()); // Returning true means that an exposed case died

            // A demographic event has occurred, so add new (i.e. next) demographic event to queue
            queuedEvents.add(new Event(EventCategory.DEMOGRAPHIC, simTime + demographicModel.nextDemogTau(mp.getMixedPop()), maxEventID));
            eo.incrementNDemographicEvents();

            // ------------- IF DEATH OF EXPOSED, REMOVE FROM exposedCases AND INFECTION from queuedEvents -------------
            // The call to this function removes one event from exposedCases in the right Population
            // and removes the corresponding infection event from queued events and replaces it with the next in the queue
            if (deathOfExposed) {
                //System.err.println("deathOfExposed: "+ deathOfExposed);
                doDeathOfExposed(focalPop);
                eo.incrementNDiedE();
            }

            // Some error checking - abort if we get an inconsistency
            if (focalPop.getTotalDogs() < 0) {
                System.err.println("Epidemic > doDemographicEvent: Negative number of dogs in population " + focalPop.toString());
                System.err.println("Event " + this.toString() + " at t=" + simTime);
                System.out.println(queuedEvents.toString());
                System.exit(0);
            }
        }

        /**
         * Retrieves all exposedCases in the focal area, selects one randomly and removes it. Then removes the corresponding
         * infection event from queuedEvents and replaces it with the next infection event from exposedCases
         *
         * @param focalPop Focal Population
         */
        public void doDeathOfExposed(Population focalPop) {
            // ------------- Remove exposed case from TreeSet of exposedCases for right area -------------
            ArrayList<Case> popCases = getCases(focalPop); // All exposed cases from the focal area
            // Choose an E from this area and remove from TreeSet of exposedCases
            int r3 = random.nextInt(popCases.size());
            Case fc = popCases.get(r3); // focal case (the one to die)

            // Assuming we got an ENDOGENOUS case, then we carry out the event (INITIAL and INCURSION cases can't die)
            if (fc.getTypeOfCase() == Case.caseType.ENDOGENOUS) {
                exposedCases.remove(fc);
                //System.out.println("An exposed case dies leaving " + exposedCases.size() + " incubating cases.");
                ///System.err.println("The exposed case was: " + fc.toString());

                // Create a record for this case in the caselist with diedE as the outcome (only if allowing endogenous to transmit, otherwise ENDOGENOUS cases not reported in caseList)
                if (allowEndogenousToTransmit) {
                    fc.getOutcomes().setDiedE(Boolean.TRUE);
                    eo.getCaseList().add(fc);
                }

                // ------------- Add the next infection event to queuedEvents -------------
                // Note that there is only one event of each type in queuedEvents. As a result, we can remove the current
                // INFECTION event and replace it with a new infection event from the updated exposedCases list.
                Event infectionEvent = getInfectionEvent();
                queuedEvents.remove(infectionEvent);
                // Add a new infection event
                if (exposedCases.size() > 0) {
                    queuedEvents.add(new Event(EventCategory.INFECTION, exposedCases.first().getDayInfectious(), maxEventID));
                }
            }

        }

        /**
         * Implements a new infection event by obtaining the first case from the exposedCases event stream and
         * rationalises event streams as appropriate
         */
        private void implementInfectionEvent() {
            // Error checking for consistency
            if (exposedCases.size() == 0) {
                System.err.println("Was expecting a case, but none found - aborting!");
                System.err.println("time:" + this.eventTime + " type:" + this.eventCategory);
                System.err.println("curr mp:" + mp.getMixedPop().toString());
                System.exit(0);
            }

            // Retrieve first exposed case, and execute running and biting, returning any secondary cases generated
            Case currCase = exposedCases.pollFirst();

            // Proceed to execute if the case type is INITIAL OR INCURSION, or if allowEndogenousToTransmit is TRUE
            if (currCase.getTypeOfCase() != Case.caseType.ENDOGENOUS || allowEndogenousToTransmit == Boolean.TRUE) {
                int pid = currCase.getPopID();
                Population p = mp.getPopulations().get(pid);
                ///System.out.println("Current population of case " + currCase.toString() + ": " + p.toString());
                ///System.out.println("rc:" + mvModel.computeExcursionRate(0, currCase.getTh(), currCase.getTd(), 1));
                ///System.out.println("excursion duration:" + mvModel.simExcursionDuration(mp.getPopulations().get(currCase.getPopID()), currCase.getTh(), currCase.getTd(), 1));
                ///System.exit(2);
                if (p.getDogsE()<0) { System.err.println("Oooopps - Exposed dogs < 0 in Epidemic > implementInfectionEvent"); System.exit(0);}

                // Do running and biting, return secondary cases
                TreeSet<Case> secCases;

                // ------- RATIONALISE exposedCases AND demographic situation --------
                // Update everything for death of this infectious case

                if (currCase.getTypeOfCase() == Case.caseType.ENDOGENOUS || currCase.getTypeOfCase() == Case.caseType.INITIAL) { // i.e. if accounted for in population counts because endogenous
                    demographicModel.doInfectiousDeath(mp.getPopulations().get(currCase.getPopID()), mp.getMixedPop());
                }

                // Generate secondary cases
                secCases = currCase.doRunningBiting(mvModel, epiModel, mp);

                // Update overall summary
                eo.getCaseList().add(currCase);
                eo.update(currCase.getOutcomes());

                // Add new cases to the eventlist, etc., assuming we are not running to test the effect of clustering
                if (!additionalString.equals("NAIVE_POP")) { // Only update eventlist or rationalise if we are NOT implementing NAIVE_POP events
                    if (secCases.size() > 0) {
                        for (Case sc : secCases) {
                            // Update exposedCases, demographic, nextID
                            sc.setId(nextID);
                            exposedCases.add(sc);
                            nextID++;
                            // Rationalise demographic situation - transition to S -> E
                            demographicModel.doInfectionEvent(mp.getPopulations().get(sc.getPopID()), mp.getMixedPop());
                        }
                    }
                }
            } else {
                // Else just do the death of this case
                if (currCase.getTypeOfCase() == Case.caseType.ENDOGENOUS || currCase.getTypeOfCase() == Case.caseType.INITIAL) { // i.e. if accounted for in population counts because endogenous
                    demographicModel.doInfectiousDeath(mp.getPopulations().get(currCase.getPopID()), mp.getMixedPop());
                }
            }

            // -------- RATIONALISE queuedEvents ---------
            // Add a new case to the queue if exposed cases remain
            if (exposedCases.size() > 0) {
                queuedEvents.add(new Event(EventCategory.INFECTION, exposedCases.first().getDayInfectious(), maxEventID));
            }

        }
    }

    /**
     * Helper method to read in initial cases information from a csv file
     *
     * @param fInitialCases csv file from which to read in: cols in file are popID, dayVaccinations, numDogsVaccinated
     */
    private void initialiseCases(String fInitialCases) {

        if (fInitialCases.length() > 0) {

            ArrayList<Case> initialCases = CaseListReader.readCSVFile(fInitialCases);

            // Add cases to the exposed list and update the demographic situation
            for (Case c : initialCases) {
                // If we're reading in real cases, these will be missing population ID and infectious period
                if (c.getInfectiousPeriod() < 0) {
                    Population p = mp.findPopulation(c.getPosition());
                    c.setPopID(p.getPopID());
                }
                exposedCases.add(c);

                System.err.println("Adding an exposed case in pop " + c.getPopID() + " of type " + c.getTypeOfCase().toString() + " with info " + c.getDayInfectious());
                if (c.getTypeOfCase() == Case.caseType.INITIAL) { // (!c.getIsExogenous()) {
                    // System.err.println("Updating demographic model in initialiseCases");
                    demographicModel.doInfectionEvent(mp.getPopulations().get(c.getPopID()), mp.getMixedPop());
                }
            }

        }
    }

    /**
     * Helper method to read in incursions information from a csv file
     *
     * @param fIncursions csv file from which to read in
     */
    private void incursions(String fIncursions) {

        if (fIncursions.length() > 0) {

            ArrayList<Case> incursions = CaseListReader.readCSVFile(fIncursions);

            for (Case c : incursions) {
                // Inclusive version of search for case location that allows us to allocate all incursions to occupied areas on-the-fly:
                c = mp.updateCaseClosestPopulationAll(c);
                Population p = mp.getPopulations().get(c.getPopID());

                if (p!=null) {
                    c.setPopID(p.getPopID());
                    //System.out.println(c.toString());
                    EpiVariableSet evs = epiModel.randEpiVariableSet();
                    c.setInfectiousPeriod(evs.getInfectiousPeriod());
                    c.setSerialInterval(evs.getSerialInterval());

                    exposedCases.add(c);
                    // NOTE THAT WE DON'T KEEP TRACK OF *EXPOSED* CASES AS PART OF POPULATION PROCESSES!!
                    // Keep track of the number of dogs incubating in each area
                } else {
                    System.out.println("Epidemic > incursions: No population found for incursion with caseID = " + c.getId());
                    System.exit(0);
                }
            }
            System.out.println("Read in " + exposedCases.size() + " initial cases + incursions to be simulated at this scale.");
        }
    }


    /**
     * Obtains all exposed cases within a given population (used by DoDeathOfExposed)
     * @param focalPop
     * @return ArrayList of object pointers to all exposed cases in a given population
     */
    private ArrayList<Case> getCases(Population focalPop) {
        ArrayList<Case> popCases = new ArrayList<Case>();
        for (Case c : exposedCases) {

            if (c.getPopID() == focalPop.getPopID()) {
                if (c.getTypeOfCase() == Case.caseType.ENDOGENOUS || c.getTypeOfCase() == Case.caseType.INITIAL)  {
                    // c.getIsExogenous() == Boolean.FALSE) {
                    popCases.add(c);
                }
            }
        }
        if (popCases.size() == 0) {
            System.err.println("No cases found in area " + focalPop.toString() + ". Expected to find one so aborting!");
            System.err.println(exposedCases.toString());
            System.exit(0);
        }
        return popCases;
    }

    /**
     * Retrieves the (first) infection event from queuedEvents (may behave strangely fail if multiple events in queued events)
     * @return
     */
    private Event getInfectionEvent() {
        // If no events left ...
        if (queuedEvents.size() == 0) {
            System.err.println("Nothing left in queuedEvents. Expected to find remaining queuedEvents so aborting!");
            System.exit(0);
        }
        // Find the infection event in queuedEvents
        Event infectionEvent = null;
        for (Event e : queuedEvents) {
            if (e.getEventCategory().equals(EventCategory.INFECTION)) {
                infectionEvent = e;
            }
        }
        // Error checking in case no infection event was found
        if (infectionEvent == null) {
            System.err.println("Didn't find an infectionEvent in queuedEvents. Expected to, so aborting!");
            System.exit(0);
        }
        return (infectionEvent);
    }

    /**
     * Sets up coverage output file writers, where required
     * @param fMixedPopCoverageOutput    File name for writing whole metapopulation coverage
     * @param fByPopCoverageOutput File name for writing coverage for each population in turn
     */
    private void initialiseCoverageOutput(String fMixedPopCoverageOutput, String fByPopCoverageOutput) {
        popCoverageTreeSet = new TreeSet<PopCoverage>();

        // If required, set up associated TreeSets and FileWriters
        if (fMixedPopCoverageOutput.length() > 0) {
            this.mixedPopCoverageWriter = new MixedPopCoverageWriter();
            popCoverageTreeSet.add(new PopCoverage(simTime, mp.getMixedPop()));
        }
        if (fByPopCoverageOutput.length() > 0) {
            this.coverageWriter = new CoverageWriter(fMixedPopCoverageOutput);
        }
    }

    /**
     * Helper method to establish if the next event will take us over a time threshold
     * @param simTime Current simulation time
     * @param eventTime Event time of next event
     * @param periodNumDays Periodicity of time thresholds in days
     * @return Boolean.TRUE if the next event takes us over time threshold; FALSE otherwise
     */
    private Boolean passedMarker(double simTime, double eventTime, double periodNumDays) {
        if (Math.floor(simTime / periodNumDays) < Math.floor(eventTime / periodNumDays)) {
            return Boolean.TRUE;
        } else {
            return Boolean.FALSE;
        }
    }

    /**
     * Appends information on current vaccination coverage at the mixedPop level
     * @param fMixedPopCoverageOutput String of length zero (no output), or filename to write to
     */
    private void outputCoverage(String fMixedPopCoverageOutput) {
        if (fMixedPopCoverageOutput.length()>0) coverageWriter.appendCsvFile(mp, Math.round(simTime) / daysInMonth);
        ///System.out.println("Writing out coverage " + fMixedPopCoverageOutput.toString());
    }

}
