/*
-- Valencia Rd: Camino De La Tierra to Mission Rd
-- eastbound
SELECT t1.SegmentID
    , t2.Name
    , t1.Bearing
    , t1.Miles
    , t1.StartLat
    , t2.Latitude
    , t1.EndLat
    , t1.StartLong
    , t2.Longitude
    , t1.EndLong
FROM [ADOT_INRIX].[dbo].[InrixSegments_Geometry] AS t1
LEFT JOIN [ADOT_INRIX].[dbo].[InrixSegments] AS t2
ON t1.SegmentID = t2.ID
WHERE ID IN ('1226208918', '1226270892')
ORDER BY Longitude

-- westbound
SELECT t1.SegmentID
    , t2.Name
    , t1.Bearing
    , t1.Miles
    , t1.StartLat
    , t2.Latitude
    , t1.EndLat
    , t1.StartLong
    , t2.Longitude
    , t1.EndLong
FROM [ADOT_INRIX].[dbo].[InrixSegments_Geometry] AS t1
LEFT JOIN [ADOT_INRIX].[dbo].[InrixSegments] AS t2
ON t1.SegmentID = t2.ID
WHERE ID IN ('1226252762', '1226269461')
ORDER BY -Longitude

-- test UTC-MST conversion
SELECT TOP 5 timestamp
    , timestamp AT TIME ZONE 'UTC' AS time_utc
    , timestamp AT TIME ZONE 'UTC' AT TIME ZONE 'US Mountain Standard Time' AS time_mst
    , DATEADD(MINUTE, DATEDIFF(MINUTE, 0, timestamp), 0) AS time_minute
    , SegmentID
FROM [ADOT_INRIX].[dbo].[Inrix_RealTime]
*/

-- speed and travel time data
SELECT time_mst
    , SegmentID
    , Bearing
    , Miles
    , speed
    , travelTimeMinutes
    , score
FROM (
    SELECT t2.timestamp AT TIME ZONE 'UTC' AT TIME ZONE 'US Mountain Standard Time' AS time_mst
        , t2.SegmentID
        , t1.Bearing
        , t1.Miles
        , t2.speed
        , t2.travelTimeMinutes
        , t2.score
    FROM [ADOT_INRIX].[dbo].[InrixSegments_Geometry] AS t1
    LEFT JOIN [ADOT_INRIX].[dbo].[Inrix_Realtime] AS t2
    ON t1.SegmentID = t2.SegmentID
    ) AS t3
WHERE SegmentID IN ('1226208918', '1226270892', '1226252762', '1226269461')
    AND DATEPART(YEAR, time_mst) = 2021
    AND DATEPART(MONTH, time_mst) IN (3, 4, 9, 10)
    AND DATEPART(WEEKDAY, time_mst) IN (2, 3, 4, 5)
ORDER BY time_mst