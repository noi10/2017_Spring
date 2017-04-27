package Assignment07;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.Set;
import java.util.HashSet;
import java.util.Map;
import java.util.HashMap;

import static Assignment07.Recommendation.*;

/**
 * DBMS Assignment 7.
 * @author Ken Baclawski
 */
public class Safari {
    /** The identifier in the Safari table. */    
    private int id = 0;  

    /** The name of the safari. */
    private String name;

    /** The database connection. */
    private Connection connection;

    /** The set of visits by the safari. */
    private Set<Visit> visits = new HashSet<Visit>();

    /** The set of provisions (materials) used by the safari. */
    private Set<Provision> provisions = new HashSet<Provision>();

    /** Map from the city identifiers to the cities. */
    private Map<Integer, City> cities = new HashMap<Integer, City>();

    /** Map from the country identifiers to the countries. */
    private Map<Integer, Country> countries = new HashMap<Integer, Country>();

    /**
     * Construct a safari using data from the database.
     * @param id The safari identifier in the database.
     * @param connection The connection to the database.
     * @throws Exception if there is no such safari or a database error occurs.
     */    
    public Safari(int id, Connection connection) throws Exception {
        this.id = id;
        this.connection = connection;
        retrieveName();
        retrieveProvisions();
        retrieveVisits();
    }

    /**
     * Retrieve the safari name from the database.
     * @throws Exception if there is no such safari or a database error occurs.
     */
    private void retrieveName() throws Exception {
	PreparedStatement retrieveSafariName = null;
        try {
            retrieveSafariName = connection.prepareStatement("select name from Safari where id = ?");
	    Utility.checkWarnings(retrieveSafariName, "Unable to prepare statement for safari name");
            retrieveSafariName.setInt(1, id);
            ResultSet results = retrieveSafariName.executeQuery();
	    Utility.checkWarnings(results, "Unable to retrieve safari name");
            while (results.next()) {
                name = results.getString(1);
                return;
            }
            throw new Exception("There is no safari with identifier " + id);
        } finally {
	    if (retrieveSafariName != null) {
		retrieveSafariName.close();
	    }
        }
    }

    /**
     * Add all provisions of the safari to the set of provisions.
     * @throws Exception if a database error occurs.
     */    
    private void retrieveProvisions() throws Exception {
	PreparedStatement retrieveProvisions = null;
        try {
            retrieveProvisions = connection.prepareStatement
                ("select p.needs, m.description, m.weight, p.quantity, p.recommendation " +
                 "  from Material m join Provision p on (m.id = p.needs) " +
                 " where p.neededBy = ?");
	    Utility.checkWarnings(retrieveProvisions, "Unable to prepare statement for retrieving provisions");
            retrieveProvisions.setInt(1, id);
            ResultSet results = retrieveProvisions.executeQuery();
	    Utility.checkWarnings(retrieveProvisions, "Unable to retrieve provisions");
            while (results.next()) {
                String recommendationString = results.getString(5);
                Recommendation recommendation = "required".equals(recommendationString)? required : optional;
                Provision provision = new Provision
                    (results.getInt(1), results.getString(2), results.getDouble(3),
                     results.getInt(4), recommendation);
                provisions.add(provision);
            }
        } finally {
	    if (retrieveProvisions != null) {
		retrieveProvisions.close();
	    }
        }
    }

    /**
     * Retrieve the visits by the safari.
     * @throws Exception if a database error occurs.
     */    
    private void retrieveVisits() throws Exception {
	PreparedStatement retrieveVisits = null;
        try {
            retrieveVisits = connection.prepareStatement
                ("select v.dayOfVisit, c.id, c.name, n.id, n.name " +
                 "  from Visit v join City c on (v.visits = c.id) " +
                 "       join Country n on (c.partOf = n.id) " +
                 " where v.visitedBy = ?");
	    Utility.checkWarnings(retrieveVisits, "Unable to prepare statement for retrieving visits");
            retrieveVisits.setInt(1, id);
            ResultSet results = retrieveVisits.executeQuery();
	    Utility.checkWarnings(results, "Unable to retrieve visits");
            while (results.next()) {

                // Retrieve the country object.

                int countryId = results.getInt(4);
                Country country = countries.get(countryId);
                if (country == null) {
                    country = new Country(results.getString(5));
                    countries.put(countryId, country);
                }

                // Retrieve the city object.

                int cityId = results.getInt(2);
                City city = cities.get(cityId);
                if (city == null) {
                    city = new City(cityId, results.getString(5), country);
                    cities.put(cityId, city);
                }

                // Construct the visit object and add it to the set of visits.
                
                Visit visit = new Visit(results.getInt(1), city);
                visits.add(visit);
	    }
        } finally {
	    if (retrieveVisits != null) {
		retrieveVisits.close();
	    }
        }
    }

    /**
     * Modify one of the provisions.
     * @param description The description of the material.
     * @param quantity The new quantity of the provision.
     * @param recommendation The new recommendation of the provision.
     * @throws Exception if a database error occurs.
     */    
    public void modifyProvision(String description, Recommendation recommendation, int quantity) throws Exception {
	for (Provision provision : provisions) {
	    if (provision.getDescription().equals(description)) {
		PreparedStatement deleteProvision = null;
		PreparedStatement insertProvision = null;
		try {
		    deleteProvision = connection.prepareStatement
			("delete from Provision where neededBy = ? and needs = ?");
		    Utility.checkWarnings(deleteProvision, "Unable to prepare statement for deleting a provision");
		    deleteProvision.setInt(1, id);
		    deleteProvision.setInt(2, provision.getMaterialId());
		    deleteProvision.executeUpdate();

		    insertProvision = connection.prepareStatement
			("insert into Provision(neededBy, needs, quantity, recommendation) values(?, ?, ?, ?)");
		    Utility.checkWarnings(insertProvision, "Unable to prepare statement for inserting a provision");
		    insertProvision.setInt(1, id);
		    insertProvision.setInt(2, provision.getMaterialId());
		    insertProvision.setInt(3, quantity);
		    insertProvision.setString(4, recommendation.toString());
		    insertProvision.executeUpdate();
		    return;
		} finally {
		    if (deleteProvision != null) {
			deleteProvision.close();
		    }
		    if (insertProvision != null) {
			insertProvision.close();
		    }
		}
	    }
	}
    }
    
    /**
     * Add a visit to the safari.
     * @param cityName The name of the city being visited.
     * @param countryName The name of the country the city is a part of.
     * @param dayOfVisit The day when the city is visited.
     */
    public void addVisit(String cityName, String countryName, int dayOfVisit) throws Exception {

	// Get the cached city using the city and country names.

	City city = getCity(cityName, countryName);

	// If there is no cached city, then create one.

	if (city == null) {

	    // Retrieve the city object from the database.

	    PreparedStatement retrieveCity = null;
	    try {
		retrieveCity = connection.prepareStatement
		    ("select c.id, n.id " +
		     "  from City c join Country n on (c.partOf = n.id) " +
		     " where c.name = ? and n.name = ?");
		Utility.checkWarnings(retrieveCity, "Unable to prepare statement for retrieving a city");
		retrieveCity.setString(1, cityName);
		retrieveCity.setString(2, countryName);
		ResultSet results = retrieveCity.executeQuery();
		Utility.checkWarnings(results, "Unable to retrieve city");
		if (results.next()) {
		    Country country = countries.get(results.getInt(2));
		    if (country == null) {
			country = new Country(countryName);
			countries.put(results.getInt(2), country);
		    }
		    int cityId = results.getInt(1);
		    city = new City(cityId, cityName, country);
		    cities.put(cityId, city);
		} else {
		    throw new Exception("There is no city named " + cityName + " in " + countryName);
		}
	    } finally {
		if (retrieveCity != null) {
		    retrieveCity.close();
		}
	    }
	}	    

	// Construct the new visit.

	Visit visit = new Visit(dayOfVisit, city);
	visits.add(visit);

	// Update the database.

	PreparedStatement addVisit = null;
	try {
	    addVisit = connection.prepareStatement
		("insert into Visit(dayOfVisit, visitedBy, visits) values(?, ?, ?)");
	    Utility.checkWarnings(addVisit, "Unable to prepare statement for inserting a new visit");
	    addVisit.setInt(1, dayOfVisit);
	    addVisit.setInt(2, id);
	    addVisit.setInt(3, city.getId());
	    addVisit.executeUpdate();
	} finally {
	    if (addVisit != null) {
		addVisit.close();
	    }
	}
    }

    /**
     * Get the name of the safari.
     * @return the safari name.
     */
    public String getName() {
	return name;
    }
    
    /**
     * Get a city from the cached cities.
     * @param cityName The name of the city.
     * @param countryName The name of the country the city is a part of.
     * @return The cached city object if there is one or null otherwise.
     */
    private City getCity(String cityName, String countryName) {
	if (cityName == null || countryName == null) {
	    return null;
	}
	for (City city : cities.values()) {
	    if (cityName.equals(city.getName())) {
		Country country = city.getPartOf();
		if (country != null) {
		    if (countryName.equals(country.getName())) {
			return city;
		    }
		}
	    }
	}
	return null;
    }
}
