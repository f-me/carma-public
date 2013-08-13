------------------------------------------------------------------------------
-- | Query to fetch partners within a box, with mobile partners coming
-- last. See 'withinPartners'.
--
-- Splice lon1, lat1 and lon2, lat2 on the query, where coordinates
-- are those of opposite 2D box points.
drop function if exists geowithin ( float, float, float,  float
                                  , float, float
                                  , text, text, text, text, text
                                  , boolean,  boolean);
create or replace function GeoWithin ( x1 float
                                     , y1 float
                                     , x2 float
                                     , y2 float
                                     , xc float
                                     -- ^ Center point longitude (0 if not used).
                                     , yc float
                                     -- ^ Center point latitude (0 if not used).
                                     , city text
                                     , makes text
                                     , services text
                                     , priority2 text
                                     , priority3 text
                                     , isDlr boolean
                                     , isMbl boolean
                                     )
returns table (res text) as
$$
  declare
    ca  text[] = string_to_array(city, ',');
    ma  text[] = string_to_array(makes, ',');
    sa  text[] = string_to_array(services, ',');
    p2a text[] = string_to_array(priority2, ',');
    p3a text[] = string_to_array(priority3, ',');
    se  boolean = array_dims(sa)  IS NULL;
    ce  boolean = array_dims(ca)  IS NULL;
    me  boolean = array_dims(ma)  IS NULL;
    p2e boolean = array_dims(p2a) IS NULL;
    p3e boolean = array_dims(p3a) IS NULL;
  begin
    return query SELECT row_to_json(r) :: text
      FROM (
        SELECT p.id
             , st_x(p.coords)
             , st_y(p.coords)
             , p.isDealer
             , p.isMobile
             , p.isFree
             , ST_Distance_Sphere(p.coords, ST_Point(xc, yc))
                                              as distance
             , coalesce(p.name, '')           as name
             , coalesce(p.city, '')           as city
             , coalesce(p.comment, '')        as comment
             , coalesce(p.code, '')           as code
             , coalesce(p.addrs  :: text, '') as addrs
             , coalesce(p.phones :: text, '') as phones
             , coalesce(p.emails :: text, '') as emails
             , coalesce(p.personInCharge, '') as personInCharge
             , p.makes                        as makes
             , coalesce(s.priority2, '')      as priority2
             , coalesce(s.priority3, '')      as priority3
             , coalesce(s.servicename, '')    as servicename
        FROM partnertbl p
        LEFT JOIN partner_servicetbl s
        ON  p.id = cast(split_part(s.parentid, ':', 2) as integer)
        AND s.parentid is not null
        AND s.parentid != ''
        WHERE coords && ST_SetSRID( ST_MakeBox2D( ST_Point(x1, y1)
                                               , ST_Point(x2, y2))
                                 , 4326)
        AND   p.isActive = 't'
        AND   (ce  OR p.city        = ANY(ca))
        AND   (se  OR s.servicename = ANY(sa))
        AND   (p2e OR s.priority2   = ANY(p2a))
        AND   (p3e OR s.priority3   = ANY(p3a))
        AND   case when isDlr then p.isDealer = true  else true end
        AND   case when isMbl then p.isMobile = isMbl else true end
        AND   case when isDealer
              then me OR p.makes && ma
              else me OR array_dims(p.makes) IS NULL OR p.makes && ma
              end
      ) as r;

  end;
$$ language plpgsql;
