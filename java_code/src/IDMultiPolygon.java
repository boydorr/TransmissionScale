/**
 * Created by rebeccamancy on 01/06/2016.
 * Allows us to use polygons with IDs
 */

import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.geom.Polygon;

public class IDMultiPolygon extends MultiPolygon {
    private int ID;

    public IDMultiPolygon(Polygon[] polygons, GeometryFactory factory, int ID) {
        super(polygons, factory);
        this.ID = ID;
    }

    public int getID() {
        return ID;
    }

    public void setID(int ID) {
        this.ID = ID;
    }
}

