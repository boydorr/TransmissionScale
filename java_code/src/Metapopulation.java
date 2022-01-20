import com.vividsolutions.jts.geom.*;
import com.vividsolutions.jts.index.strtree.STRtree;
import com.vividsolutions.jts.io.WKTFileReader;
import com.vividsolutions.jts.io.WKTReader;
import com.vividsolutions.jts.io.WKTWriter;
import jdistlib.rng.RandomEngine;
import org.apache.commons.math3.stat.descriptive.rank.Median;
import org.nocrala.tools.gis.data.esri.shapefile.exception.InvalidShapeFileException;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.*;

/**
 * Created by rebeccamancy on 25/04/2015.
 * A Metapopulation object has the following notable attributes:
 *  - An ArrayList of Population objects (dogs with their status information)
 *  - A GeometryCollection object holding the spatial information (grid cell boundaries read in from a file in WKT format)
 *  - A TreeSet of VaccinationCampaigns
 *  - A STRtree spatial index (helps to find the gridcell associated with a point)
 */
public class Metapopulation {
    private final static String CSV_DELIMITER = ", *|\\n|\\n\\r";      // delimiters in csv file are new lines and commas + arbitrary num of spaces
    private final static double LOG_PLUS = 0.1;

    private String mpName;             // e.g. Serengeti
    private GeometryCollection area;   // Store as a GeometryCollection
    private Geometry unionStudyArea;
    private GeometryFactory factory = new GeometryFactory();
    private final double M2_PER_KM2 = 1000*1000;
    private double cellAreaSqM; // Gridcell area in metres for normalising dog densities
    private RandomEngine random;
    private TreeSet<Double> updateKDays = new TreeSet<Double>(); // Days from start to update the carrying capacities to simulate population growth
    private HashMap<Double, StandardisationParameters> stdDensity = new HashMap<Double, StandardisationParameters>(); // parameters (mean and sd) for standarising dog density
    private ArrayList<Population> populations = new ArrayList<Population>(); // Main data structure of interest
    private double medianSize; // Median population size
    private HashMap<String, SpatialUnit> spatialUnitHashMap = new HashMap(); // HashMap correspondence from name of village to SpatialUnits
    private List<Geometry> geomList;
    private List<IDMultiPolygon> idMultiPolygonArrayList;
    private STRtree si = new STRtree();  // Spatial index
    private Population mixedPop; // This is equivalent to the combined total population (as if it were fully mixed)
    private TreeSet<VaccinationCampaign> vaccinationCampaignTreeset = new TreeSet<VaccinationCampaign>();

    public String getMpName() {
        return mpName;
    }
    public double getKM2() {
        return unionStudyArea.getArea() / M2_PER_KM2;
    }
    public int getNumGeometries() {
        return area.getNumGeometries();
    }
    public TreeSet<Double> getUpdateKDays() { return updateKDays; }

    public HashMap<String, SpatialUnit> getspatialUnitHashMap() { return spatialUnitHashMap; }

    /**
     * Constructs a Metapopulation as a collection of Population objects
     * @param fPops
     * @param fWKT
     * @param mpName
     * @throws IOException
     * @throws InvalidShapeFileException
     * @throws com.vividsolutions.jts.io.ParseException
     */
    public Metapopulation(String fPops, String fWKT, String mpName, double cellAreaSqM, RandomEngine random, Random rgen) throws IOException,
            InvalidShapeFileException,
            com.vividsolutions.jts.io.ParseException {

        this.mpName = mpName;
        this.cellAreaSqM = cellAreaSqM;
        this.random = random;

        // Use helper methods below to set mp attributes by reading in populations and geometries, vaccination campaigns etc.
        System.out.println(fPops);
        populations = readTimeConsistentPopulations(fPops);
        readGeometries(fWKT);   // Exits if different number relative to populations

        // Add all populations to spatialUnitHashMap (so that we can group populations by village if required) (not used in manuscript)
        for (Population pop : populations) {
            if (!spatialUnitHashMap.containsKey(pop.getName())) {
                spatialUnitHashMap.put(pop.getName(), new SpatialUnit(pop.getName(), this.random));
            }
            spatialUnitHashMap.get(pop.getName()).addPopulation(pop);
        }

        // Combine all populations to get the fully-mixed equivalent, and compute the union of all areas
        combinePops();
        unionStudyArea = area.union();
        setMedianPopulationSize();

        // Work out the spatial envelope (used for generating random points within it)
        // Spatial envelope of whole study area
        Envelope envelope = enclosingEnvelopFromGeometry(unionStudyArea);
        System.out.println("In Metapopulation, and have just read in " + this.getPopulations().size()+ " populations with a total area of : " + this.getKM2() + " Sq Km");
    }


    /**
     * Sets the standardisation parameters for standardising dog densities
     */
    public void setStdDensity() {
        for (Double update : updateKDays) {
            stdDensity.put(update, new StandardisationParameters(update));
        }
    }

    /**
     * Computes time-consistent density
     * @param popID
     * @param updateKDays
     * @return
     */
    public double computeTimeConsistentDensity ( int popID, double updateKDays ) {
        if (updateKDays >= 5112) { // if run over the end of the timeframe
            updateKDays = 5082.0;
        }
        return stdDensity.get(updateKDays).getPopDensity(popID);
    }

    /**
     * Computes standardised log density of a given population a specific point in time
     * @param popID
     * @param updateKDays
     * @return
     */
    public double computeStandardisedLogDensity ( int popID, double updateKDays) {
        if (updateKDays >= 5112) { // if run over the end of the timeframe
            updateKDays = 5082.0;
        }
        double timeConsistentLogDensity = Math.log(computeTimeConsistentDensity(popID, updateKDays) + LOG_PLUS);
        return (timeConsistentLogDensity - stdDensity.get(updateKDays).getMeanLogDensity()) / stdDensity.get(updateKDays).getSdLogDensity();
    }

    /**
     * Returns an ArrayList of MultiPolygon type objects, that each has an ID
     * @param gc GeometryCollection object
     * @return
     */
    private List<IDMultiPolygon> convertToIDMultipolygons(GeometryCollection gc, ArrayList<Population> pops) {

        // Check that there are the same number of entries in the populations as in the GeometryCollection
        if (gc.getNumGeometries() != pops.size()) {
            System.err.println("The numbers of geometries and populations do not correspond. Aborting.");
            System.exit(0);
        }

        List<IDMultiPolygon> idMultiPolygonArrayList = new ArrayList<IDMultiPolygon>();

        // Loop over entries in the GeometryCollection
        for (int k=0; k < gc.getNumGeometries(); k++) {
            Geometry current = gc.getGeometryN(k); // This will be a Polygon or Multipolygon
            int howmany = current.getNumGeometries();

            // Convert into an array of Polygon objects required for IDMultiPolygon Constructor
            Polygon[] polygonsArray = new Polygon[howmany];
            for (int ind=0; ind < howmany; ind++) {
                polygonsArray[ind] = (Polygon)current.getGeometryN(ind);
            }

            // Use this array of Polygon to create an IDMultiPolygon, with ID from pops
            IDMultiPolygon currIDMultipolygon = new IDMultiPolygon(polygonsArray, factory, pops.get(k).getPopID() );
            idMultiPolygonArrayList.add(k, currIDMultipolygon);

        }
        return idMultiPolygonArrayList;
    }

    /**
     * Generate a new spatial index
     * @param idMultiPolygonList
     * @return
     */
    private STRtree generateSI(List<IDMultiPolygon> idMultiPolygonList) {
        STRtree si = new STRtree();
        for (IDMultiPolygon mp : idMultiPolygonList) {
            si.insert(mp.getEnvelopeInternal(), mp);
        }
        return si;
    }

    /**
     * Compute the spatial envelope of a Geometry object
     * Taking inspiration from https://stackoverflow.com/questions/8520692/minimal-bounding-rectangle-with-jts
     * @param geometry
     * @return
     */
    public static Envelope enclosingEnvelopFromGeometry(Geometry geometry) {
        final Envelope envelope = new Envelope();
        final Geometry enclosingGeometry = geometry.getEnvelope();
        final Coordinate[] enclosingCoordinates = enclosingGeometry.getCoordinates();
        for (Coordinate c : enclosingCoordinates) {
            envelope.expandToInclude(c);
        }
        return envelope;
    }


    public void setVaccinationCampaigns(String folder, String fCampaigns) {
        if (fCampaigns.length()>0) {
            readVaccinationCampaignsPopulation(folder + fCampaigns);
        }
    }

    public Population getMixedPop() {
        return mixedPop;
    }

    public TreeSet<VaccinationCampaign> getVaccinationCampaignTreeset () {
        return vaccinationCampaignTreeset;
    }

    /**
     * Helper method to read in geometries / shapes / etc. from Well-known-text WKT format version of shapefile
     * @param fWKT
     * @throws IOException
     * @throws InvalidShapeFileException
     * @throws com.vividsolutions.jts.io.ParseException
     */
    private void readGeometries(String fWKT) throws IOException, InvalidShapeFileException,
            com.vividsolutions.jts.io.ParseException{

        // The environment / shapes / Geometries
        WKTReader reader = new WKTReader(factory);
        File fh = new File(fWKT);
        WKTFileReader wktfr = new WKTFileReader(fh, reader);
        // Read WKT into an (ordered) List of Geometries. However, this actually only contains one list element of type GeometryCollection, so we then extract this.
        geomList = wktfr.read();
        area = (GeometryCollection)geomList.get(0);

        // Check that the number of geometries is the same as the number of population (If OK, set the geometry associated with each population)
        if (populations.size() != area.getNumGeometries())  {
            System.err.println("In Metapopulation > readGeometries: The number of geometries (shape file areas)=" + area.getNumGeometries() + " does not match the number of populations=" + populations.size() + " read in. Aborting.");
            System.exit(0);
        } else {
            for (int i = 0; i < populations.size(); i++) {
                populations.get(i).setGeometry(area.getGeometryN(i));   // Attributing a Multipolygon or a Polygon to each Population
            }
        }

        // Set up the idMultiPolygonArrayList to be able to use the Spatial Index
        idMultiPolygonArrayList = convertToIDMultipolygons(area, populations);
        si = generateSI(idMultiPolygonArrayList);

    }

    /**
     * Resets the metapopulation so all dogs statuses are Susceptible (basically re-initialisation)
     * and reset median population sizes and demographic rates, and K values (in case of growth)
     */
    public void reset(DemographicModel dm) {
        for (Population p : populations) {
            p.reset();
            p.setK(p.getInitialK(), dm); // Reset K to initial value
        }
        dm.computeDemographicRates(this);
        setMedianPopulationSize();
    }


    /**
     * Helper method to read in vaccination campaign information
     * @param fVC filename of vaccination campaign data
     */
    private void readVaccinationCampaignsPopulation (String fVC) {

        if (fVC.length()>0) {
            Scanner sc = null;
            String auName;
            int popID, numDogsVaccinated;
            double dayVaccinations; // what we'll be reading in
            final String NEW_LINE_SEPARATOR = "\n";
            // Initialise scanner for reading in
            try {
                sc = new Scanner(new File(fVC));
                sc.useDelimiter(NEW_LINE_SEPARATOR);   // delimiters in csv file are new lines and commas + arbitrary num of spaces
                String header = sc.next();
                ///System.err.print("VC header read in: "); System.err.println(header);
                String csvHeader = "auName,dayVaccinations,popID,DogsVaccinated";
                ///System.err.println("VC header required: " + csvHeader);
                if (header.compareTo(csvHeader) != 0) {
                    System.err.println("Header read in was incorrect: " + header + " Reconfigure populations file with " + csvHeader + " and try again!");
                    System.exit(0);
                }

                // Now read in items separated by commas
                int nReadIn = 0;
                sc.useDelimiter(CSV_DELIMITER);   // delimiters in csv file are new lines and commas + arbitrary num of spaces

                while (sc.hasNext()) {
                    auName = sc.next();
                    dayVaccinations = Integer.parseInt(sc.next());
                    popID = Integer.parseInt(sc.next());
                    numDogsVaccinated = Integer.parseInt(sc.next());
                    // System.out.println(popID + " " + dayVaccinations + " " + numDogsVaccinated + " " + vaccinationCampaignTreeset.size());
                    vaccinationCampaignTreeset.add(new VaccinationCampaign(auName, dayVaccinations, popID, numDogsVaccinated, vaccinationCampaignTreeset.size()));
                    nReadIn ++;
                }
                System.out.println("Read in "+ nReadIn + " campaigns and treeset has " + vaccinationCampaignTreeset.size() + " entries");
                ///System.out.println(vaccinationCampaignTreeset.toString());

            } catch (FileNotFoundException ex) {
                ex.printStackTrace();
                System.exit(1);
            } finally {
                if (sc != null) {
                    try {
                        sc.close();
                    } catch (Exception ex) {
                        ex.printStackTrace();
                        System.exit(1);
                    }
                }
            }
        }
    }

    /**
     * Helper method to read in time-consistent population
     * @param fPops Filename
     * @return
     */
    private ArrayList<Population> readTimeConsistentPopulations(String fPops) {

        ArrayList<Population> populations = new ArrayList<Population>();

        if (fPops.length()>0) {
            ///System.out.println(fPops);
            Scanner sc = null;

            // Data we will read in: auName and popID, plus all the population sizes
            String auName; int popID;
            final String NEW_LINE_SEPARATOR = "\n";

            // Initialise scanner for reading in
            try {
                sc = new Scanner(new File(fPops));
                sc.useDelimiter(NEW_LINE_SEPARATOR);   // delimiters in csv file are new lines and commas + arbitrary num of spaces

                // HEADER ROW (column names)
                String header = sc.next(); // Read in first line as a single string
                String[] arrHeader = header.split(",");
                int nDates = arrHeader.length - 2; // First two columns of csv are for auName and popID

                // Convert to dates when updating will occur to double format
                for (int d = 0; d < nDates; d++) {
                    this.updateKDays.add(Double.parseDouble(arrHeader[d+2]));
                }

                // DATA - read in items separated by commas
                sc.useDelimiter(CSV_DELIMITER);   // delimiters in csv file are new lines and commas + arbitrary num of spaces

                double[] popSizes = new double[nDates];
                int nReadIn = 0;
                while (sc.hasNext()) {
                    // Each new population
                    auName = sc.next(); // System.out.println("Reading in metapopulation pops and auName: " + auName + " next item " + sc.next() + " " + CSV_DELIMITER);
                    popID = Integer.parseInt(sc.next());
                    for (int d = 0; d < nDates; d++) { // Loop over dates
                        popSizes[d] = Double.parseDouble(sc.next());
                    }

                    if (nReadIn==0 && popID != 0) {
                        System.out.println("Line of populations file " + fPops + " " + header + " : " + popID + " " + auName);
                        System.err.println("Csv file containing populations should start with popId=0"); System.exit(0);
                    }

                    // Add to the populations list
                    populations.add(new Population(popID, auName, updateKDays, popSizes));
                    nReadIn ++;
                }

            } catch (FileNotFoundException ex) {
                ex.printStackTrace();
                System.exit(1);
            } finally {
                if (sc != null) {
                    try {
                        sc.close();
                    } catch (Exception ex) {
                        ex.printStackTrace();
                        System.exit(1);
                    }
                }
            }
        }
        return(populations);
    }

    /**
     * Combine all populations to give the fully-mixed equivalent population
     */
    private void combinePops (){
        // Set up the fully-mixed population version of the metapopulation
        Population mix = new Population(0);  // initialise with zero dogs
        for (Population pop : populations) {
            mix = mix.combinePops(pop);   // a method of Population
        }
        mix.setName("MIXED");
        this.mixedPop = mix;
    }

    public void combinePopsAgain (){
        // Re-do the fully-mixed population version of the metapopulation
        Population mix = new Population(0);      // initialise with zero dogs
        for (Population pop : populations) {
            mix = mix.combinePops(pop);
        }
        mix.setName("MIXED");
        this.mixedPop = mix;
    }

    public void updateKMonthly(double humanMonthlyGrowthRate, DemographicModel dm) {
        for (Population p : populations){
            p.setK(humanMonthlyGrowthRate * p.getK(), dm);
            p.setEventRate(dm.eventRate(p));
        }
        setMedianPopulationSize(); // recompute median population size
    }

    /**
     * Returns the Population associated with a Point, or null if either pt or the Population is null
     * @param pt
     * @return
     */
    public Population findPopulation(Point pt) {

        Population pop = null;

        if (pt != null) {
            // If we have few populations, then search by normal iteration over all populations
            if (this.populations.size() < 100) {
                for (Population p : populations) {
                    Boolean contains = p.getGeo().contains(pt);
                    if (contains) {
                        pop = p;
                        break;
                    }
                } // Still null if not found by here
            // If we have lots of populations, then use the spatial index and then check intersecting populations - for speed
            } else {
                // Construct an Envelope object required for querying the spatial index
                Envelope ee = new Envelope(new Coordinate(pt.getX(), pt.getY()));
                List<IDMultiPolygon> polygonsFoundList = si.query(ee);

                // Check which of the returned spatial indices actually intersects with this point
                // NB: in the case of squares/rectangles, there should be no overlap, but other shapes can lead to multiple spatial indices being returned.
                if (polygonsFoundList.size() == 0) { // If no intersecting polygons, return null (so that we can redraw, if generating new cases endogenously)
                    // System.out.println("Metapopulation > findPopulation: Case left the area or otherwise outside");
                    return null;
                } else if (polygonsFoundList.size() > 1) { // If multiple polygons found
                    for (IDMultiPolygon p : polygonsFoundList) {
                        Population popTest = populations.get(p.getID()); // pop = popTest;
                        System.out.println("Mp > findPopulation: popTest=" + popTest);
                        Boolean contains = popTest.getGeo().contains(pt);

                        if (contains) {
                            pop = popTest;
                            break;
                        }
                    } /// Still null if not found by here
                } else { // Found one only, so return this
                    return populations.get(polygonsFoundList.get(0).getID());
                }

            }
        }
        return pop; // NB: returns null if pt was null
    }

    /**
     * Returns the Population associated with a Point (used in post-processing - gets *nearest* if not actually within polygons)
     * @param pt
     * @return
     */
    public Population findPopulationAll(Point pt) {

        if (pt != null) {
            // Construct an Envelope object required for querying the spatial index
            Envelope ee = new Envelope(new Coordinate(pt.getX(), pt.getY()));
            List<IDMultiPolygon> polygonsFoundList = si.query(ee);

            if (polygonsFoundList.size() > 1) {
                System.err.println("Point " + pt.toString() + " found in multiple populations. Aborting");
                System.exit(0);
            } else if (polygonsFoundList.size() == 1) {
                // System.out.println("Found point " + pt.toString() + " and allocated to population " + populations.get(polygonsFoundList.get(0).getID()).getPopID() + " of mp" + this.mpName);
                return populations.get(polygonsFoundList.get(0).getID());
            } else if (polygonsFoundList.size() == 0) {
                System.out.println("In Metapopulation > findPopulationAll and trying to find population; and no population currently found ... trying buffer");
                double buffer = 0;
                List<IDMultiPolygon> polygonsFoundListBuffered = new ArrayList<IDMultiPolygon>();
                while (polygonsFoundListBuffered.size() == 0) {
                    buffer ++;
                    // Try expanding the envelope to make it intersect with one of the shapes
                    double x1 = pt.getX() - buffer;
                    double x2 = pt.getX() + buffer;
                    double y1 = pt.getY() - buffer;
                    double y2 = pt.getY() + buffer;
                    Envelope eee = new Envelope(x1, x2, y1, y2);
                    polygonsFoundListBuffered = si.query(eee);
                    if (polygonsFoundListBuffered.size() > 0) {
                        ///System.out.println("Using envelope expansion to find point " + pt.toString() + " and allocated to population " + populations.get(polygonsFoundListBuffered.get(0).getID()).getPopID() + " of mp" + this.mpName + " using a buffer of size " + buffer + "m");
                        return populations.get(polygonsFoundListBuffered.get(0).getID());
                    }
                }

            }
        }
        // If we didn't find the polygon or pt was null, return null
        return null;

    }

    /**
     * Returns a Case, updated with new popID and location, getting the *nearest* if not actually within polygons.
     *     Allocates to a random point within the closest polygon in the mp
     * @param c a Case
     * @return
     */
    public Case updateCaseClosestPopulationAll(Case c) {

        Point pt = c.getPosition();
        /// System.out.println("caseID = " + c.getId());

        if (pt != null) {
            // Construct an Envelope object required for querying the spatial index
            Envelope ee = new Envelope(new Coordinate(pt.getX(), pt.getY()));
            List<IDMultiPolygon> polygonsFoundList = si.query(ee);

            if (polygonsFoundList.size() > 1) {
                System.err.println("Point " + pt.toString() + " found in multiple populations. Aborting");
                System.exit(0);
            } else if (polygonsFoundList.size() == 1) { // Population has been found
                /// System.out.println("Found point " + pt.toString() + " and allocated to population " + populations.get(polygonsFoundList.get(0).getID()).getPopID() + " of mp" + this.mpName);
                // Set population id
                Population cPop = populations.get(polygonsFoundList.get(0).getID());
                c.setPopID(cPop.getPopID());
                return c;
            } else { // outwith area (polygonsFoundList.size == 0)
                /// System.out.println("In Metapopulation > updateCaseClosestPopulationAll and trying to find population; and no population currently found ... trying buffer");
                double buffer = 0;
                List<IDMultiPolygon> polygonsFoundListBuffered = new ArrayList<IDMultiPolygon>();
                while (polygonsFoundListBuffered.size() == 0) {
                    buffer ++;
                    // Try expanding the envelope to make it intersect with one of the shapes
                    double x1 = pt.getX() - buffer;
                    double x2 = pt.getX() + buffer;
                    double y1 = pt.getY() - buffer;
                    double y2 = pt.getY() + buffer;
                    Envelope eee = new Envelope(x1, x2, y1, y2);
                    polygonsFoundListBuffered = si.query(eee);
                    if (polygonsFoundListBuffered.size() > 0) { // Once we find a polygon
                        ///System.out.println("Using envelope expansion to find point " + pt.toString() + " and allocated to population " + populations.get(polygonsFoundListBuffered.get(0).getID()).getPopID() + " of mp" + this.mpName + " using a buffer of size " + buffer + "m");
                        Population cPop = populations.get(polygonsFoundListBuffered.get(0).getID());
                        c.setPositionAjusted(Boolean.TRUE);
                        c.setPopID(cPop.getPopID());
                        // Set coordinates to a random point in the population
                        Point newPt = generateRandomPoint(cPop);
                        c.setPosition(newPt);
                        /// System.out.println(c.toString());
                        return c;
                    }
                }

            }
        }
        // If we didn't find the polygon or case was null, return null
        return null;
    }


    /**
     * Selects a random population proportional to carrying capacity K of each
     * @return
     */
    public Population getRandPopProportionalK() {
        // First select the population proportional to population sizes
        double rand = random.nextDouble() * mixedPop.getK();  // between 0 and total K
        double curr = 0;  Population pp = null;
        for (Population p : populations){
            curr = curr + p.getK();
            if (curr > rand) {
                pp = p;
                break;
            }
        }
        if (pp == null) {
            System.out.println("Developer error - pp is null in Metapopulation>getRandPopProportionalK");
            System.exit(0);
        }
        return(pp);
    }

    /**
     * Selects a random population proportional to log(K+1) in each
     * @return
     */
    public Population getRandPopProportionalLogK() {
        // Sum of logged population sizes
        double sumlog = 0;
        for (Population p : populations){
            sumlog = sumlog + Math.log1p(p.getK());
        }
        // First select the population proportional to population sizes
        double rand = random.nextDouble();  // between 0 and 1
        double curr = 0;  Population pp = null;
        for (Population p : populations){
            curr = curr + (Math.log1p(p.getK())/sumlog);
            if (curr > rand) {
                pp = p;
                break;
            }
        }
        if (pp == null) {
            System.out.println("Developer error - pp is null in Metapopulation>getRandPopProportionalK");
            System.exit(0);
        }
        if (pp.getK() == 0.0) {
            System.out.println("Chose a population with zero carrying capacity!"); System.exit(0);
        }
        return(pp);
    }

    /**
     * Selects a population uniform randomly among those with K greater than above
     * @param above
     * @return
     */
    public Population getRandPopAboveX(double above) {
        // First select the population proportional to population sizes
        double rand = random.nextDouble() * this.populations.size();  // between 0 and total number of populations
        double curr = 0;  Population pp = null;
        while(pp == null) {
            for (Population p : populations){
                curr = curr + 1;
                if (curr > rand && p.getK() > above) {
                    pp = p;
                    break;
                }
            }
        }
        return(pp);
    }

    /**
     * Compute median population size
     */
    public void setMedianPopulationSize () {

        Median med = new Median();
        double[] popSizes = new double[this.getPopulations().size()];
        int counter = 0;
        for (Population pop : this.getPopulations()) {
            popSizes[counter] = pop.getK();
        }
        double medianValue = med.evaluate(popSizes);
        this.medianSize = medianValue;
    }

    public double getMedianSize() {
        return medianSize;
    }

    /**
     * Generates a random point within the simple geometry associated with a population
     * @param pop
     * @return
     */
    public Point generateRandomPoint(Population pop) {

        Point pt = null;
        Envelope popEnv = enclosingEnvelopFromGeometry(pop.getGeo());;

        while (findPopulation(pt) != pop) {
            double newX = popEnv.getMinX() + (popEnv.getMaxX() - popEnv.getMinX()) * random.nextDouble();
            double newY = popEnv.getMinY() + (popEnv.getMaxY() - popEnv.getMinY()) * random.nextDouble();
            //System.out.println("New random point X=" + newX + ", Y=" + newY);
            pt = factory.createPoint(new Coordinate(newX, newY));
        }
        return pt;
    }

    /**
     * Generate a random point in a Metapopulation object according to the appropriate algorithm
     * @param type
     * @return
     */
    public Point generateRandomPoint(String type) {
        // First, generate a population according to appropriate algorithm
        Population currPop = null;
        if (type.equals("randUnif")) {
            currPop = this.getRandPopAboveX(0); // unif random across cells with K>0
        } else if (type.equals("randPropK")) {
            currPop = this.getRandPopProportionalK();
        } else if (type.equals("randPropLogK")) {
            currPop = this.getRandPopProportionalLogK();
        } else if (type.equals("randUnifAboveMedian")) {
            currPop = this.getRandPopAboveX(this.getMedianSize());
        } else {
            System.err.println("From Metapopulation > generateRandomPoint(String type): initialLocationsType parameter = " + type + " not recognised.");
            System.exit(0);
        }

        Point pt = generateRandomPoint(currPop);
        return(pt);
    }

    /**
     * Converts Metapopulation object to a string in WKT format for textual output
     * @return
     */
    public String toString() {
        String wkt = "";
        WKTWriter writer = new WKTWriter();
        for (int i=0; i < area.getNumGeometries(); i++) {
            wkt = wkt + " " + writer.write(area.getGeometryN(i));
        }
        return wkt;
    }

    public String toStringPopns () {
        String str = "";
        ListIterator<Population> populationListIterator = populations.listIterator();
        while (populationListIterator.hasNext()) {
            str = str + populationListIterator.next().toCSVString() + "\n";
        }
        return str;
    }

    public ArrayList<Population> getPopulations() {
        return populations;
    }

    /**
     * Subclass for the parameters used to standardise the dog density (at appropriate scaling).
     * Consists of mean and standard deviation of the density across cells. There is an instance of this object per date.
     */
    private class StandardisationParameters {
        private double meanLogDensity, sdLogDensity;
        private HashMap<Integer, Double> popSizes, popDensities, logPopDensities; // density per sq km

        /**
         * Constructor to make standardisation parameters for one timepoint. This is used from PostProcessor
         */
        public StandardisationParameters(Double daysK) {

            popSizes = new HashMap<Integer, Double>(populations.size());
            popDensities = new HashMap<Integer, Double>(populations.size());
            logPopDensities = new HashMap<Integer, Double>(populations.size());

            // Put all population sizes into hashmaps with popID as the key
            for (Population p : populations) {
                    double pK = p.getMapDaysK().get(daysK); // Returns K on that date for that population
                    double pDensity = pK * (M2_PER_KM2 / cellAreaSqM); // density per sq km
                    popSizes.put(p.getPopID(), pK);
                    popDensities.put(p.getPopID(), pDensity);
                    logPopDensities.put(p.getPopID(), Math.log(pDensity + LOG_PLUS)); // Returns log(K) on that date for that population
            }
            // Compute mean and sd
            Statistics s = new Statistics(logPopDensities);
            meanLogDensity = s.getMean();
            sdLogDensity = s.getStdDev();

        }

        // Getter methods
        public double getMeanLogDensity() { return meanLogDensity; }
        public double getSdLogDensity() { return sdLogDensity; }
        public double getPopDensity (int popID) { return popDensities.get(popID); }

    }

}
