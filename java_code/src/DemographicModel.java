// This version current at around January 2018

import jdistlib.rng.RandomEngine;

/**
 * Created by rebeccamancy on 13/08/2015.
 */
public class DemographicModel {
    RandomEngine random;

    private double perCapBirthRate, perCapDeathRate;
    private String cellSideLengthM; // for explicit manipulation of rates by cell area spatial scale

    public DemographicModel(double perCapBirthRate, double perCapDeathRate, String cellSideLengthM, RandomEngine random) {
        this.perCapBirthRate = perCapBirthRate;
        this.perCapDeathRate = perCapDeathRate;
        this.random = random;
        this.cellSideLengthM = cellSideLengthM;
    }

    /**
     * Gets the delay (waiting time) until next demographic event, based on population (or mixedpop) rate
     * @param pop
     * @return Simulated waiting time
     */
    public double nextDemogTau(Population pop) {
        return Util.randExp(pop.getEventRate(), random);
    }

    /**
     * Implements the next demographic event
     * @param pop Population in which event occurs
     * @param mixedPop Population object that contains the fully-mixed equivalent of the metapopulation
     * @return Boolean.TRUE if death of an exposed has occurred so we can rationalise with list of incubating cases in Epidemic
     */
    public Boolean doEvent(Population pop, Population mixedPop) {
        // Get previous rates to allow us to compute updated rates after event is executed
        double prevEventRateMP = mixedPop.getEventRate();
        double prevEventRateP = pop.getEventRate();
        Boolean returnVal = Boolean.FALSE;

        // Error checking if carrying capacity is zero for the selected population
        if (pop.getK() == 0.0) {
            System.out.println("DM > doEvent > focal pop " + pop.toString());
            System.out.println("DM > doEvent > rate " + (pop.getBirthRate() + pop.getDeathRate()));
            System.out.println("DM > doEvent. Trying to execute a demographic event in a population with K=0.0");
            System.exit(0);
        }

        // Choose birth or death event probabilistically proportional to rates for this (sub)population
        double r = random.nextDouble(); // Allows us to choose between birth and death for rates along a unit interval


        if ( r < ( computeAugmentationRatePop(pop) / (pop.getEventRate()) )) {
            // BIRTH: If r in the first part of the interval, we execute a birth
            pop.doBirth();
            mixedPop.doBirth();

        } else {
            // DEATH: so need to choose a state from which to die

            if(pop.getTotalDogs()== 0) { // Error checking
                System.out.println("Ooops, no dogs to die in population " + pop.toString()); System.exit(0);
            }

            // Choose by computing rates along a unit interval, and execute event
            double r2 = random.nextDouble();
            if ( r2 < ((double)pop.getDogsS() / (double)pop.getTotalDogs()) ) {
                pop.doDeath(Epidemic.epiStatus.S);
                mixedPop.doDeath(Epidemic.epiStatus.S);           // Susceptible death
            } else if (r2 < ((double)(pop.getDogsS()+pop.getDogsV()) / (double)pop.getTotalDogs())) {
                pop.doDeath(Epidemic.epiStatus.V);
                mixedPop.doDeath(Epidemic.epiStatus.V);           // Vaccinated death
            } else {   // Must be death of an exposed
                pop.doDeath(Epidemic.epiStatus.E);
                mixedPop.doDeath(Epidemic.epiStatus.E);           // Death of Exposed
                returnVal = Boolean.TRUE;
            }
        }
        // Update the demographic rates in the relevant Population and mp
        pop.setEventRate(eventRate(pop));
        pop.setBirthRate(computeAugmentationRatePop(pop));
        pop.setDeathRate(computeDeathRatePop(pop));
        mixedPop.setEventRate(prevEventRateMP + (pop.getEventRate()-prevEventRateP));
        return returnVal;
    }

    /**
     * Implements a vaccination campaign event
     * @param pop
     * @param mixedPop
     * @param numDogs
     */
    public void doVaccinationEvent(Population pop, Population mixedPop, int numDogs) {
        /// System.out.println("DM > doVaccinationEvent in population " + pop.toString() + ", vaccinating " + numDogs + " dogs.");
        int totDogs = pop.getTotalDogs();
        if (totDogs > 0) {    // If no dogs, don't do the vaccination
            // If we appear to be vaccinating more dogs than we have in this population, set to the total number of dogs there
            if (numDogs > totDogs) {
                numDogs = totDogs;
                // System.out.println("Not enough dogs to vaccinate in population " + pop.toString() + " so vaccinating total number of " + totDogs);
            }
            // Share out numDogs between S, V and E compartments (NB we don't implement transmission if already in V or E category, but assume vaccination does not prevent disease progression from E to I if already exposed at this point)
            int numSDogs = (int) ((double)numDogs * ((double)pop.getDogsS() / (double)totDogs));
            pop.doVaccination(numSDogs);
            mixedPop.doVaccination(numSDogs);
        } else {
            /// System.out.println("No dogs to vaccinate in population " + pop.toString());
        }
    }

    /**
     * Implements an infection event at both population and metapopulation level
     * @param pop
     * @param mixedPop
     */
    public void doInfectionEvent(Population pop, Population mixedPop) {
        pop.doInfection();
        mixedPop.doInfection();
    }

    /**
     * Implements the death of an infectious dog
     * @param pop
     * @param mixedPop
     */
    public void doInfectiousDeath(Population pop, Population mixedPop) {
        double prevEventRateMP = mixedPop.getEventRate();
        double prevEventRateP = pop.getEventRate();
        // Action the death
        pop.doDeath(Epidemic.epiStatus.E);
        mixedPop.doDeath(Epidemic.epiStatus.E);

        // Reset pop and mp demographic event rate after death
        pop.setEventRate(eventRate(pop));
        pop.setBirthRate(computeAugmentationRatePop(pop));
        pop.setDeathRate(computeDeathRatePop(pop));
        mixedPop.setEventRate(prevEventRateMP + (pop.getEventRate() - prevEventRateP));   // NB: assume any rounding errors are minimal
    }


    /**
     * Computes the rate of "population growth" ("population augmentation") due to births and acquisitions
     * Dogs are assumed to be acquired (by humans) at a mean rate of 1/(4.3*30) (such that 1/2 of all dogs
     *      are replaced within 3 months), or at a rate given by multiplying the standing population size by the
     *      per capita natural birth rate (set equal to the per capita death rate), whichever was higher.
     * This approach is chosen to model a constant acquisition rate by humans per dog below the carrying capacity,
     *      until there are sufficient dogs that the local natural population birth rate exceeds the acquisition rate
     *      and thus takes over. Above the carrying capacity, births occurs in a density-dependent manner:
     *      birthRate * N * (1.0 - ((N-K)/K) ).
     *
     * @param pop
     * @return
     */
    public double computeAugmentationRatePop(Population pop) {

        int N = pop.getTotalDogs();
        double acquisitionRate = (1/(4.3*30)), k = pop.getK(), kAdjust, augmentationRatePop;

        // The basic demographic model gives inflated population sizes when k is small, and too small when k large
        // This adjustment of the carrying capacity accounts for this, so that at all scales, growth works similarly
        kAdjust = 0.96*Math.pow(k,1.018)-0.11;
        if (cellSideLengthM.equals("all")) {
            kAdjust = 0.995 * kAdjust;
        } else if (cellSideLengthM.equals("4000")) {
            kAdjust = 1.065 * kAdjust;
        } else if (cellSideLengthM.equals("2000")) {
            kAdjust = 1.055 * kAdjust;
        } else if (cellSideLengthM.equals("1000")) {
            kAdjust = 1.028 * kAdjust;
        } else if (cellSideLengthM.equals("250")) {
            kAdjust = 0.97 * kAdjust;
        }

        double birthRatePop = 0, acquisitionRatePop = 0;

        // Note that it's important to return 0 if k is 0; also return 0 if population is above twice the carrying capacity
        if (k == 0.0 || kAdjust == 0.0 || N >= 2*kAdjust) {
            augmentationRatePop = 0.0;
        } else {
            if ((double)N < kAdjust) { // Below the carrying capacity
                acquisitionRatePop = Math.max(0, ((kAdjust-(double)N) * acquisitionRate) ); // Population replacement rate as maximum of zero and acquisitions at constant rate per depleted dog
                if (N == 0) {
                    // Only acquisition possible
                    augmentationRatePop = acquisitionRatePop;
                } else {
                    // Births at a rate given by a scaled maximum per capita growth rate, plus acquisitions at constant rate per depleted dog
                    birthRatePop = perCapDeathRate * (double)N;
                    augmentationRatePop = Math.max(acquisitionRatePop, birthRatePop);
                    ///if (!(birthRatePop >=0.0)) System.out.println("N < k but > 0");
                }
            } else { // i.e. N >= k  ... at or above the carrying capacity (up to 2*K)
                // Only births (no acquisition). Birth rate decreases logistically with N, equals death rate at carrying capacity
                birthRatePop = perCapDeathRate * (double)N * (1.0 - (((double)N-kAdjust) / kAdjust) );
                augmentationRatePop = birthRatePop;
                ///if (!(birthRatePop >=0.0)) System.out.println("N >= k " + birthRatePop + " N=" + N + ", kAdjust= " + kAdjust);
            }
        }
        // Output an error and quit if we get a problem with augmentation rate
        if (!(augmentationRatePop >= 0.0)) System.err.println("Help! Augmentation rate problem! " + augmentationRatePop
              + ", acquisitionRatePop=" + acquisitionRatePop + ", birthRatePop=" + birthRatePop);

        return augmentationRatePop;
    }

    /**
     * Compute population level death rate from per capita rate
     * @param pop
     * @return
     */
    public double computeDeathRatePop(Population pop) {
        return perCapDeathRate * (pop.getTotalDogs());
    }

    /**
     * Basically a "getter" that computes total demographic event rate (births/acquisitions + deaths)
     * @param pop
     * @return
     */
    public double eventRate(Population pop) {
        return computeAugmentationRatePop(pop) + computeDeathRatePop(pop);
    }


    /**
     * Selects a population proportional to the total rate of each population
     * @param mp
     * @return
     */
    public Population propRatePop (Metapopulation mp) {
        double rand = random.nextDouble() * mp.getMixedPop().getEventRate();
        Population found = mp.getPopulations().get(mp.getPopulations().size()-1); // set as the last population initially
        double currVal = 0;
        for (Population pop : mp.getPopulations()) {
            currVal = currVal + pop.getEventRate();
            if (currVal > rand) {
                found = pop;
                break;
            }
        }
        /// System.out.println("DM > propRatePop > rand=" + rand + " mp.getMixedPop().getEventRate()=" + mp.getMixedPop().getEventRate());
        return found;
    }

    /**
     * Computes demographic rates, by population and overall
     * @param mp
     */
    public void computeDemographicRates(Metapopulation mp) {
        double totRate = 0;
        double br, dr;
        for (Population pop : mp.getPopulations()) {
            br = computeAugmentationRatePop(pop);
            dr = computeDeathRatePop(pop);
            pop.setBirthRate(br);
            pop.setDeathRate(dr);
            pop.setEventRate(br+dr);
            totRate = totRate + br + dr;
        }
        mp.getMixedPop().setEventRate(totRate);

    }

}
