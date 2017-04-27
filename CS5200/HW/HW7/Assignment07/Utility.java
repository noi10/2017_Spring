package Assignment07;

import java.sql.Statement;
import java.sql.ResultSet;
import java.sql.Connection;
import java.sql.SQLWarning;

/**
 * Utility for checking SQL warnings.
 * @author Ken Baclawski
 */
public class Utility {
    /**
     * Print exceptions.
     * @param exception The exception to be printed.
     * @param message Optional message to show with the exception.
     */
    private static void printException(Exception exception, String message) {
        if (message != null) {
            System.out.println(message + " due to: " + exception);
        } else {
            System.out.println(exception);
        }
    }

    /**
     * Print the warnings.
     * @param warning The first warning to be printed.
     * @param message Optional message to show with the warnings.
     */
    private static void printWarnings(SQLWarning warning, String message) {
        try {
            while (warning != null) {
                printException(warning, message);
                warning = warning.getNextWarning();
            }
        } catch (Exception exception) {
            printException(exception, message);
        }
    }

    /**
     * Check for statement warnings.
     * @param statement The JDBC statement being checked.  This should always
     * be a prepared statement.
     * @param message Optional message to show with the warnings.
     */
    public static void checkWarnings(Statement statement, String message) {
        try {
            if (statement == null || statement.isClosed()) {
                return;
            }
            printWarnings(statement.getWarnings(), message);
        } catch (Exception exception) {
            printException(exception, message);
        }
    }

    /**
     * Check for result set warnings.
     * @param results The JDBC result set being checked.  This should always
     * be a prepared statement.
     * @param message Optional message to show with the warnings.
     */
    public static void checkWarnings(ResultSet results, String message) {
        try {
            if (results == null || results.isClosed()) {
                return;
            }
            printWarnings(results.getWarnings(), message);
        } catch (Exception exception) {
            printException(exception, message);
        }
    }

    /**
     * Check for connection warnings.
     * @param connection The JDBC connection being checked.  This should always
     * be a prepared statement.
     * @param message Optional message to show with the warnings.
     */
    public static void checkWarnings(Connection connection, String message) {
        try {
            if (connection == null || connection.isClosed()) {
                return;
            }
            printWarnings(connection.getWarnings(), message);
        } catch (Exception exception) {
            printException(exception, message);
        }
    }
}
