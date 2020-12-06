import warnings
import math

warnings.simplefilter("always")


def check_standard_compliance(standard, **kwargs):
    params = dict()
    params["standard"] = standard
    for key, value in kwargs.items():
        params[key] = value

    if params["standard"] == "utci":
        for key, value in params.items():
            if key == "v":
                if value > 17:
                    warnings.warn(
                        "UTCI wind speed applicability limits between 0.5 and 17 m/s",
                        UserWarning,
                    )
                elif value < 0.5:
                    warnings.warn(
                        "UTCI wind speed applicability limits between 0.5 and 17 m/s",
                        UserWarning,
                    )

    elif params["standard"] == "ashrae":  # based on table 7.3.4 ashrae 55 2017
        for key, value in params.items():
            if key in ["tdb", "tr"]:
                if key == "tdb":
                    parameter = "dry-bulb"
                else:
                    parameter = "mean radiant"
                if value > 40 or value < 10:
                    warnings.warn(
                        f"ASHRAE {parameter} temperature applicability limits between 10 and 40 °C",
                        UserWarning,
                    )
            if key in ["v", "vr"]:
                if value > 2 or value < 0:
                    warnings.warn(
                        "ASHRAE air velocity applicability limits between 0 and 2 m/s",
                        UserWarning,
                    )
            if key == "met":
                if value > 2 or value < 1:
                    warnings.warn(
                        "ASHRAE met applicability limits between 1.0 and 2.0 met",
                        UserWarning,
                    )
            if key == "clo":
                if value > 1.5 or value < 0:
                    warnings.warn(
                        "ASHRAE clo applicability limits between 0.0 and 1.5 clo",
                        UserWarning,
                    )
            if key == "v_limited":
                if value > 0.2:
                    raise ValueError(
                        "This equation is only applicable for air speed lower than 0.2 m/s"
                    )

    elif params["standard"] == "iso":  # based on ISO 7730:2005 page 3
        for key, value in params.items():
            if key == "tdb":
                if value > 30 or value < 10:
                    warnings.warn(
                        "ISO air temperature applicability limits between 10 and 30 °C",
                        UserWarning,
                    )
            if key == "tr":
                if value > 40 or value < 10:
                    warnings.warn(
                        "ISO mean radiant temperature applicability limits between 10 and 40 °C",
                        UserWarning,
                    )
            if key in ["v", "vr"]:
                if value > 1 or value < 0:
                    warnings.warn(
                        "ISO air velocity applicability limits between 0 and 1 m/s",
                        UserWarning,
                    )
            if key == "met":
                if value > 4 or value < 0.8:
                    warnings.warn(
                        "ISO met applicability limits between 0.8 and 4.0 met",
                        UserWarning,
                    )
            if key == "clo":
                if value > 2 or value < 0:
                    warnings.warn(
                        "ISO clo applicability limits between 0.0 and 2 clo",
                        UserWarning,
                    )


def utci(tdb, tr, v, rh, units="SI"):
    """ Determines the Universal Thermal Climate Index (UTCI). The UTCI is the
    equivalent temperature for the environment derived from a reference environment.
    It is defined as the air temperature of the reference environment which produces
    the same strain index value in comparison with the reference individual's response
    to the real
    environment. It is regarded as one of the most comprehensive indices for
    calculating heat stress in outdoor spaces. The parameters that are taken into
    account for calculating
    UTCI involve dry-bulb temperature, mean radiation temperature, the pressure of
    water vapor or relative humidity, and wind speed (at the elevation of 10 m) [7]_.

    Parameters
    ----------
    tdb : float
        dry bulb air temperature, default in [°C] in [°F] if `units` = 'IP'
    tr : float
        mean radiant temperature, default in [°C] in [°F] if `units` = 'IP'
    v : float
        relative air velocity, default in [m/s] in [fps] if `units` = 'IP'
    rh : float
        relative humidity, [%]
    units: str default="SI"
        select the SI (International System of Units) or the IP (Imperial Units) system.

    Returns
    -------
    utci : float
         Universal Thermal Climate Index, [°C] or in [°F]

    Notes
    -----
    You can use this function to calculate the Universal Thermal Climate Index (`UTCI`)
    The applicability wind speed value must be between 0.5 and 17 m/s.

    .. _UTCI: http://www.utci.org/utcineu/utcineu.php

    Examples
    --------
    .. code-block:: python

        >>> from pythermalcomfort.models import utci
        >>> utci(tdb=25, tr=25, v=1.0, rh=50)
        24.6

        >>> # for users who wants to use the IP system
        >>> utci(tdb=77, tr=77, v=3.28, rh=50, units='ip')
        76.4

    Raises
    ------
    ValueError
        Raised if the input are outside the Standard's applicability limits

    """

    if units.lower() == "ip":
        tdb, tr, v = units_converter(tdb=tdb, tr=tr, v=v)

    #check_standard_compliance(standard="utci", tdb=tdb, tr=tr, v=v)

    def es(tdb):
        g = [
            -2836.5744,
            -6028.076559,
            19.54263612,
            -0.02737830188,
            0.000016261698,
            (7.0229056 * (10 ** (-10))),
            (-1.8680009 * (10 ** (-13))),
        ]
        tk = tdb + 273.15  # air temp in K
        es = 2.7150305 * math.log1p(tk)
        for count, i in enumerate(g):
            es = es + (i * (tk ** (count - 2)))
        es = math.exp(es) * 0.01  # convert Pa to hPa
        return es

    # Do a series of checks to be sure that the input values are within the bounds
    # accepted by the model.
    
    if (
        (tdb < -50.0)
        or (tdb > 50.0)
        or (tr - tdb < -30.0)
        or (tr - tdb > 70.0)
        or (v < 0.5)
        or (v > 17)
    ):
        return(None)
        raise ValueError(
            "The value you entered are outside the equation applicability limits"
        )
    # This is a python version of the UTCI_approx function
    # Version a 0.002, October 2009
    # tdb: air temperature, degrees Celsius
    # ehPa: water vapour presure, hPa=hecto Pascal
    # Tmrt: mean radiant temperature, degrees Celsius
    # va10m: wind speed 10m above ground level in m/s

    ehPa = es(tdb) * (rh / 100.0)
    delta_t_tr = tr - tdb
    Pa = ehPa / 10.0  # convert vapour pressure to kPa

    utci_approx = (
        tdb
        + (0.607562052)
        + (-0.0227712343) * tdb
        + (8.06470249 * (10 ** (-4))) * tdb * tdb
        + (-1.54271372 * (10 ** (-4))) * tdb * tdb * tdb
        + (-3.24651735 * (10 ** (-6))) * tdb * tdb * tdb * tdb
        + (7.32602852 * (10 ** (-8))) * tdb * tdb * tdb * tdb * tdb
        + (1.35959073 * (10 ** (-9))) * tdb * tdb * tdb * tdb * tdb * tdb
        + (-2.25836520) * v
        + (0.0880326035) * tdb * v
        + (0.00216844454) * tdb * tdb * v
        + (-1.53347087 * (10 ** (-5))) * tdb * tdb * tdb * v
        + (-5.72983704 * (10 ** (-7))) * tdb * tdb * tdb * tdb * v
        + (-2.55090145 * (10 ** (-9))) * tdb * tdb * tdb * tdb * tdb * v
        + (-0.751269505) * v * v
        + (-0.00408350271) * tdb * v * v
        + (-5.21670675 * (10 ** (-5))) * tdb * tdb * v * v
        + (1.94544667 * (10 ** (-6))) * tdb * tdb * tdb * v * v
        + (1.14099531 * (10 ** (-8))) * tdb * tdb * tdb * tdb * v * v
        + (0.158137256) * v * v * v
        + (-6.57263143 * (10 ** (-5))) * tdb * v * v * v
        + (2.22697524 * (10 ** (-7))) * tdb * tdb * v * v * v
        + (-4.16117031 * (10 ** (-8))) * tdb * tdb * tdb * v * v * v
        + (-0.0127762753) * v * v * v * v
        + (9.66891875 * (10 ** (-6))) * tdb * v * v * v * v
        + (2.52785852 * (10 ** (-9))) * tdb * tdb * v * v * v * v
        + (4.56306672 * (10 ** (-4))) * v * v * v * v * v
        + (-1.74202546 * (10 ** (-7))) * tdb * v * v * v * v * v
        + (-5.91491269 * (10 ** (-6))) * v * v * v * v * v * v
        + (0.398374029) * delta_t_tr
        + (1.83945314 * (10 ** (-4))) * tdb * delta_t_tr
        + (-1.73754510 * (10 ** (-4))) * tdb * tdb * delta_t_tr
        + (-7.60781159 * (10 ** (-7))) * tdb * tdb * tdb * delta_t_tr
        + (3.77830287 * (10 ** (-8))) * tdb * tdb * tdb * tdb * delta_t_tr
        + (5.43079673 * (10 ** (-10))) * tdb * tdb * tdb * tdb * tdb * delta_t_tr
        + (-0.0200518269) * v * delta_t_tr
        + (8.92859837 * (10 ** (-4))) * tdb * v * delta_t_tr
        + (3.45433048 * (10 ** (-6))) * tdb * tdb * v * delta_t_tr
        + (-3.77925774 * (10 ** (-7))) * tdb * tdb * tdb * v * delta_t_tr
        + (-1.69699377 * (10 ** (-9))) * tdb * tdb * tdb * tdb * v * delta_t_tr
        + (1.69992415 * (10 ** (-4))) * v * v * delta_t_tr
        + (-4.99204314 * (10 ** (-5))) * tdb * v * v * delta_t_tr
        + (2.47417178 * (10 ** (-7))) * tdb * tdb * v * v * delta_t_tr
        + (1.07596466 * (10 ** (-8))) * tdb * tdb * tdb * v * v * delta_t_tr
        + (8.49242932 * (10 ** (-5))) * v * v * v * delta_t_tr
        + (1.35191328 * (10 ** (-6))) * tdb * v * v * v * delta_t_tr
        + (-6.21531254 * (10 ** (-9))) * tdb * tdb * v * v * v * delta_t_tr
        + (-4.99410301 * (10 ** (-6))) * v * v * v * v * delta_t_tr
        + (-1.89489258 * (10 ** (-8))) * tdb * v * v * v * v * delta_t_tr
        + (8.15300114 * (10 ** (-8))) * v * v * v * v * v * delta_t_tr
        + (7.55043090 * (10 ** (-4))) * delta_t_tr * delta_t_tr
        + (-5.65095215 * (10 ** (-5))) * tdb * delta_t_tr * delta_t_tr
        + (-4.52166564 * (10 ** (-7))) * tdb * tdb * delta_t_tr * delta_t_tr
        + (2.46688878 * (10 ** (-8))) * tdb * tdb * tdb * delta_t_tr * delta_t_tr
        + (2.42674348 * (10 ** (-10))) * tdb * tdb * tdb * tdb * delta_t_tr * delta_t_tr
        + (1.54547250 * (10 ** (-4))) * v * delta_t_tr * delta_t_tr
        + (5.24110970 * (10 ** (-6))) * tdb * v * delta_t_tr * delta_t_tr
        + (-8.75874982 * (10 ** (-8))) * tdb * tdb * v * delta_t_tr * delta_t_tr
        + (-1.50743064 * (10 ** (-9))) * tdb * tdb * tdb * v * delta_t_tr * delta_t_tr
        + (-1.56236307 * (10 ** (-5))) * v * v * delta_t_tr * delta_t_tr
        + (-1.33895614 * (10 ** (-7))) * tdb * v * v * delta_t_tr * delta_t_tr
        + (2.49709824 * (10 ** (-9))) * tdb * tdb * v * v * delta_t_tr * delta_t_tr
        + (6.51711721 * (10 ** (-7))) * v * v * v * delta_t_tr * delta_t_tr
        + (1.94960053 * (10 ** (-9))) * tdb * v * v * v * delta_t_tr * delta_t_tr
        + (-1.00361113 * (10 ** (-8))) * v * v * v * v * delta_t_tr * delta_t_tr
        + (-1.21206673 * (10 ** (-5))) * delta_t_tr * delta_t_tr * delta_t_tr
        + (-2.18203660 * (10 ** (-7))) * tdb * delta_t_tr * delta_t_tr * delta_t_tr
        + (7.51269482 * (10 ** (-9))) * tdb * tdb * delta_t_tr * delta_t_tr * delta_t_tr
        + (9.79063848 * (10 ** (-11)))
        * tdb
        * tdb
        * tdb
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        + (1.25006734 * (10 ** (-6))) * v * delta_t_tr * delta_t_tr * delta_t_tr
        + (-1.81584736 * (10 ** (-9))) * tdb * v * delta_t_tr * delta_t_tr * delta_t_tr
        + (-3.52197671 * (10 ** (-10)))
        * tdb
        * tdb
        * v
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        + (-3.36514630 * (10 ** (-8))) * v * v * delta_t_tr * delta_t_tr * delta_t_tr
        + (1.35908359 * (10 ** (-10)))
        * tdb
        * v
        * v
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        + (4.17032620 * (10 ** (-10)))
        * v
        * v
        * v
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        + (-1.30369025 * (10 ** (-9)))
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        + (4.13908461 * (10 ** (-10)))
        * tdb
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        + (9.22652254 * (10 ** (-12)))
        * tdb
        * tdb
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        + (-5.08220384 * (10 ** (-9)))
        * v
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        + (-2.24730961 * (10 ** (-11)))
        * tdb
        * v
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        + (1.17139133 * (10 ** (-10)))
        * v
        * v
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        + (6.62154879 * (10 ** (-10)))
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        + (4.03863260 * (10 ** (-13)))
        * tdb
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        + (1.95087203 * (10 ** (-12)))
        * v
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        + (-4.73602469 * (10 ** (-12)))
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        + (5.12733497) * Pa
        + (-0.312788561) * tdb * Pa
        + (-0.0196701861) * tdb * tdb * Pa
        + (9.99690870 * (10 ** (-4))) * tdb * tdb * tdb * Pa
        + (9.51738512 * (10 ** (-6))) * tdb * tdb * tdb * tdb * Pa
        + (-4.66426341 * (10 ** (-7))) * tdb * tdb * tdb * tdb * tdb * Pa
        + (0.548050612) * v * Pa
        + (-0.00330552823) * tdb * v * Pa
        + (-0.00164119440) * tdb * tdb * v * Pa
        + (-5.16670694 * (10 ** (-6))) * tdb * tdb * tdb * v * Pa
        + (9.52692432 * (10 ** (-7))) * tdb * tdb * tdb * tdb * v * Pa
        + (-0.0429223622) * v * v * Pa
        + (0.00500845667) * tdb * v * v * Pa
        + (1.00601257 * (10 ** (-6))) * tdb * tdb * v * v * Pa
        + (-1.81748644 * (10 ** (-6))) * tdb * tdb * tdb * v * v * Pa
        + (-1.25813502 * (10 ** (-3))) * v * v * v * Pa
        + (-1.79330391 * (10 ** (-4))) * tdb * v * v * v * Pa
        + (2.34994441 * (10 ** (-6))) * tdb * tdb * v * v * v * Pa
        + (1.29735808 * (10 ** (-4))) * v * v * v * v * Pa
        + (1.29064870 * (10 ** (-6))) * tdb * v * v * v * v * Pa
        + (-2.28558686 * (10 ** (-6))) * v * v * v * v * v * Pa
        + (-0.0369476348) * delta_t_tr * Pa
        + (0.00162325322) * tdb * delta_t_tr * Pa
        + (-3.14279680 * (10 ** (-5))) * tdb * tdb * delta_t_tr * Pa
        + (2.59835559 * (10 ** (-6))) * tdb * tdb * tdb * delta_t_tr * Pa
        + (-4.77136523 * (10 ** (-8))) * tdb * tdb * tdb * tdb * delta_t_tr * Pa
        + (8.64203390 * (10 ** (-3))) * v * delta_t_tr * Pa
        + (-6.87405181 * (10 ** (-4))) * tdb * v * delta_t_tr * Pa
        + (-9.13863872 * (10 ** (-6))) * tdb * tdb * v * delta_t_tr * Pa
        + (5.15916806 * (10 ** (-7))) * tdb * tdb * tdb * v * delta_t_tr * Pa
        + (-3.59217476 * (10 ** (-5))) * v * v * delta_t_tr * Pa
        + (3.28696511 * (10 ** (-5))) * tdb * v * v * delta_t_tr * Pa
        + (-7.10542454 * (10 ** (-7))) * tdb * tdb * v * v * delta_t_tr * Pa
        + (-1.24382300 * (10 ** (-5))) * v * v * v * delta_t_tr * Pa
        + (-7.38584400 * (10 ** (-9))) * tdb * v * v * v * delta_t_tr * Pa
        + (2.20609296 * (10 ** (-7))) * v * v * v * v * delta_t_tr * Pa
        + (-7.32469180 * (10 ** (-4))) * delta_t_tr * delta_t_tr * Pa
        + (-1.87381964 * (10 ** (-5))) * tdb * delta_t_tr * delta_t_tr * Pa
        + (4.80925239 * (10 ** (-6))) * tdb * tdb * delta_t_tr * delta_t_tr * Pa
        + (-8.75492040 * (10 ** (-8))) * tdb * tdb * tdb * delta_t_tr * delta_t_tr * Pa
        + (2.77862930 * (10 ** (-5))) * v * delta_t_tr * delta_t_tr * Pa
        + (-5.06004592 * (10 ** (-6))) * tdb * v * delta_t_tr * delta_t_tr * Pa
        + (1.14325367 * (10 ** (-7))) * tdb * tdb * v * delta_t_tr * delta_t_tr * Pa
        + (2.53016723 * (10 ** (-6))) * v * v * delta_t_tr * delta_t_tr * Pa
        + (-1.72857035 * (10 ** (-8))) * tdb * v * v * delta_t_tr * delta_t_tr * Pa
        + (-3.95079398 * (10 ** (-8))) * v * v * v * delta_t_tr * delta_t_tr * Pa
        + (-3.59413173 * (10 ** (-7))) * delta_t_tr * delta_t_tr * delta_t_tr * Pa
        + (7.04388046 * (10 ** (-7))) * tdb * delta_t_tr * delta_t_tr * delta_t_tr * Pa
        + (-1.89309167 * (10 ** (-8)))
        * tdb
        * tdb
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * Pa
        + (-4.79768731 * (10 ** (-7))) * v * delta_t_tr * delta_t_tr * delta_t_tr * Pa
        + (7.96079978 * (10 ** (-9)))
        * tdb
        * v
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * Pa
        + (1.62897058 * (10 ** (-9)))
        * v
        * v
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * Pa
        + (3.94367674 * (10 ** (-8)))
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * Pa
        + (-1.18566247 * (10 ** (-9)))
        * tdb
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * Pa
        + (3.34678041 * (10 ** (-10)))
        * v
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * Pa
        + (-1.15606447 * (10 ** (-10)))
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * Pa
        + (-2.80626406) * Pa * Pa
        + (0.548712484) * tdb * Pa * Pa
        + (-0.00399428410) * tdb * tdb * Pa * Pa
        + (-9.54009191 * (10 ** (-4))) * tdb * tdb * tdb * Pa * Pa
        + (1.93090978 * (10 ** (-5))) * tdb * tdb * tdb * tdb * Pa * Pa
        + (-0.308806365) * v * Pa * Pa
        + (0.0116952364) * tdb * v * Pa * Pa
        + (4.95271903 * (10 ** (-4))) * tdb * tdb * v * Pa * Pa
        + (-1.90710882 * (10 ** (-5))) * tdb * tdb * tdb * v * Pa * Pa
        + (0.00210787756) * v * v * Pa * Pa
        + (-6.98445738 * (10 ** (-4))) * tdb * v * v * Pa * Pa
        + (2.30109073 * (10 ** (-5))) * tdb * tdb * v * v * Pa * Pa
        + (4.17856590 * (10 ** (-4))) * v * v * v * Pa * Pa
        + (-1.27043871 * (10 ** (-5))) * tdb * v * v * v * Pa * Pa
        + (-3.04620472 * (10 ** (-6))) * v * v * v * v * Pa * Pa
        + (0.0514507424) * delta_t_tr * Pa * Pa
        + (-0.00432510997) * tdb * delta_t_tr * Pa * Pa
        + (8.99281156 * (10 ** (-5))) * tdb * tdb * delta_t_tr * Pa * Pa
        + (-7.14663943 * (10 ** (-7))) * tdb * tdb * tdb * delta_t_tr * Pa * Pa
        + (-2.66016305 * (10 ** (-4))) * v * delta_t_tr * Pa * Pa
        + (2.63789586 * (10 ** (-4))) * tdb * v * delta_t_tr * Pa * Pa
        + (-7.01199003 * (10 ** (-6))) * tdb * tdb * v * delta_t_tr * Pa * Pa
        + (-1.06823306 * (10 ** (-4))) * v * v * delta_t_tr * Pa * Pa
        + (3.61341136 * (10 ** (-6))) * tdb * v * v * delta_t_tr * Pa * Pa
        + (2.29748967 * (10 ** (-7))) * v * v * v * delta_t_tr * Pa * Pa
        + (3.04788893 * (10 ** (-4))) * delta_t_tr * delta_t_tr * Pa * Pa
        + (-6.42070836 * (10 ** (-5))) * tdb * delta_t_tr * delta_t_tr * Pa * Pa
        + (1.16257971 * (10 ** (-6))) * tdb * tdb * delta_t_tr * delta_t_tr * Pa * Pa
        + (7.68023384 * (10 ** (-6))) * v * delta_t_tr * delta_t_tr * Pa * Pa
        + (-5.47446896 * (10 ** (-7))) * tdb * v * delta_t_tr * delta_t_tr * Pa * Pa
        + (-3.59937910 * (10 ** (-8))) * v * v * delta_t_tr * delta_t_tr * Pa * Pa
        + (-4.36497725 * (10 ** (-6))) * delta_t_tr * delta_t_tr * delta_t_tr * Pa * Pa
        + (1.68737969 * (10 ** (-7)))
        * tdb
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * Pa
        * Pa
        + (2.67489271 * (10 ** (-8)))
        * v
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * Pa
        * Pa
        + (3.23926897 * (10 ** (-9)))
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * Pa
        * Pa
        + (-0.0353874123) * Pa * Pa * Pa
        + (-0.221201190) * tdb * Pa * Pa * Pa
        + (0.0155126038) * tdb * tdb * Pa * Pa * Pa
        + (-2.63917279 * (10 ** (-4))) * tdb * tdb * tdb * Pa * Pa * Pa
        + (0.0453433455) * v * Pa * Pa * Pa
        + (-0.00432943862) * tdb * v * Pa * Pa * Pa
        + (1.45389826 * (10 ** (-4))) * tdb * tdb * v * Pa * Pa * Pa
        + (2.17508610 * (10 ** (-4))) * v * v * Pa * Pa * Pa
        + (-6.66724702 * (10 ** (-5))) * tdb * v * v * Pa * Pa * Pa
        + (3.33217140 * (10 ** (-5))) * v * v * v * Pa * Pa * Pa
        + (-0.00226921615) * delta_t_tr * Pa * Pa * Pa
        + (3.80261982 * (10 ** (-4))) * tdb * delta_t_tr * Pa * Pa * Pa
        + (-5.45314314 * (10 ** (-9))) * tdb * tdb * delta_t_tr * Pa * Pa * Pa
        + (-7.96355448 * (10 ** (-4))) * v * delta_t_tr * Pa * Pa * Pa
        + (2.53458034 * (10 ** (-5))) * tdb * v * delta_t_tr * Pa * Pa * Pa
        + (-6.31223658 * (10 ** (-6))) * v * v * delta_t_tr * Pa * Pa * Pa
        + (3.02122035 * (10 ** (-4))) * delta_t_tr * delta_t_tr * Pa * Pa * Pa
        + (-4.77403547 * (10 ** (-6))) * tdb * delta_t_tr * delta_t_tr * Pa * Pa * Pa
        + (1.73825715 * (10 ** (-6))) * v * delta_t_tr * delta_t_tr * Pa * Pa * Pa
        + (-4.09087898 * (10 ** (-7)))
        * delta_t_tr
        * delta_t_tr
        * delta_t_tr
        * Pa
        * Pa
        * Pa
        + (0.614155345) * Pa * Pa * Pa * Pa
        + (-0.0616755931) * tdb * Pa * Pa * Pa * Pa
        + (0.00133374846) * tdb * tdb * Pa * Pa * Pa * Pa
        + (0.00355375387) * v * Pa * Pa * Pa * Pa
        + (-5.13027851 * (10 ** (-4))) * tdb * v * Pa * Pa * Pa * Pa
        + (1.02449757 * (10 ** (-4))) * v * v * Pa * Pa * Pa * Pa
        + (-0.00148526421) * delta_t_tr * Pa * Pa * Pa * Pa
        + (-4.11469183 * (10 ** (-5))) * tdb * delta_t_tr * Pa * Pa * Pa * Pa
        + (-6.80434415 * (10 ** (-6))) * v * delta_t_tr * Pa * Pa * Pa * Pa
        + (-9.77675906 * (10 ** (-6))) * delta_t_tr * delta_t_tr * Pa * Pa * Pa * Pa
        + (0.0882773108) * Pa * Pa * Pa * Pa * Pa
        + (-0.00301859306) * tdb * Pa * Pa * Pa * Pa * Pa
        + (0.00104452989) * v * Pa * Pa * Pa * Pa * Pa
        + (2.47090539 * (10 ** (-4))) * delta_t_tr * Pa * Pa * Pa * Pa * Pa
        + (0.00148348065) * Pa * Pa * Pa * Pa * Pa * Pa
    )

    # cmf = utci_approx > 9 and utci_approx < 26
    #
    # if utci_approx < -14.0:
    #     stress_range = -2
    # elif utci_approx < 9.0:
    #     stress_range = -1
    # elif utci_approx < 26.0:
    #     stress_range = 0
    # elif utci_approx < 32.0:
    #     stress_range = 1
    # else:
    #     stress_range = 2

    # return {'utci': round(UTCI_approx, 1), 'cmf': cmf, 'stress_range': stress_range}
    if units.lower() == "ip":
        utci_approx = units_converter(tmp=utci_approx, from_units="si")[0]

    return round(utci_approx, 1)

def units_converter(from_units="ip", **kwargs):
    """ Converts IP values to SI units

    Parameters
    ----------
    from_units: str
        specify system to convert from
    **kwargs : [t, v]

    Returns
    -------
    converted values in SI units
    """
    results = list()
    if from_units == "ip":
        for key, value in kwargs.items():
            if "tmp" in key or key == "tr" or key == "tdb":
                results.append((value - 32) * 5 / 9)
            if key in ["v", "vr", "vel"]:
                results.append(value / 3.281)
            if key == "area":
                results.append(value / 10.764)
            if key == "pressure":
                results.append(value * 101325)

    elif from_units == "si":
        for key, value in kwargs.items():
            if "tmp" in key or key == "tr" or key == "tdb":
                results.append((value * 9 / 5) + 32)
            if key in ["v", "vr", "vel"]:
                results.append(value * 3.281)
            if key == "area":
                results.append(value * 10.764)
            if key == "pressure":
                results.append(value / 101325)

    return results

def solar_gain(
    sol_altitude,
    sol_azimuth,
    sol_radiation_dir,
    sol_transmittance,
    f_svv,
    f_bes,
    asw=0.7,
    posture="seated",
    floor_reflectance=0.6,
):
    """
        Calculates the solar gain to the human body using the Effective Radiant Field (
        ERF) [1]_. The ERF is a measure of the net energy flux to or from the human body.
        ERF is expressed in W over human body surface area [w/m2]. In addition,
        it calculates the delta mean radiant temperature. Which is the amount by which
        the mean radiant
        temperature of the space should be increased if no solar radiation is present.

        Parameters
        ----------
        sol_altitude : float
            Solar altitude, degrees from horizontal [deg]. Ranges between 0 and 90.
        sol_azimuth : float
            Solar azimuth, degrees clockwise from North [deg]. Ranges between 0 and 180.
        posture : str
            Default 'seated' list of available options 'standing', 'supine' or 'seated'
        sol_radiation_dir : float
            Direct-beam solar radiation, [W/m2]. Ranges between 200 and 1000. See Table
            C2-3 of ASHRAE 55 2017 [1]_.
        sol_transmittance : float
            Total solar transmittance, ranges from 0 to 1. The total solar
            transmittance of window systems, including glazing unit, blinds, and other
            façade treatments, shall be determined using one of the following methods:
            i) Provided by manufacturer or from the National Fenestration Rating
            Council approved Lawrence Berkeley National Lab International Glazing
            Database.
            ii) Glazing unit plus venetian blinds or other complex or unique shades
            shall be calculated using National Fenestration Rating Council approved
            software or Lawrence Berkeley National Lab Complex Glazing Database.
        f_svv : float
            Fraction of sky vault exposed to body, ranges from 0 to 1.
        f_bes : float
            Fraction of the possible body surface exposed to sun, ranges from 0 to 1.
            See Table C2-2 and equation C-7 ASHRAE 55 2017 [1]_.
        asw: float
            The average short-wave absorptivity of the occupant. It will range widely,
            depending on the color of the occupant’s skin as well as the color and
            amount of clothing covering the body.
            A value of 0.7 shall be used unless more specific information about the
            clothing or skin color of the occupants is available.
            Note: Short-wave absorptivity typically ranges from 0.57 to 0.84, depending
            on skin and clothing color. More information is available in Blum (1945).
        floor_reflectance: float
            Floor refectance. It is assumed to be constant and equal to 0.6.

        Notes
        -----
        More information on the calculation procedure can be found in Appendix C of [1]_.

        Returns
        -------
        erf: float
            Solar gain to the human body using the Effective Radiant Field [W/m2]
        delta_mrt: float
            Delta mean radiant temperature. The amount by which the mean radiant
            temperature of the space should be increased if no solar radiation is present.

        Examples
        --------
        .. code-block:: python

            >>> from pythermalcomfort.models import solar_gain
            >>> results = solar_gain(sol_altitude=0, sol_azimuth=120,
            sol_radiation_dir=800, sol_transmittance=0.5, f_svv=0.5, f_bes=0.5,
            asw=0.7, posture='seated')
            >>> print(results)
            {'erf': 42.9, 'delta_mrt': 10.3}

        """

    posture = posture.lower()
    if posture not in ["standing", "supine", "seated"]:
        raise ValueError("Posture has to be either standing, supine or seated")

    def find_span(arr, x):
        for i in range(0, len(arr)):
            if arr[i + 1] >= x >= arr[i]:
                return i
        return -1

    deg_to_rad = 0.0174532925
    hr = 6
    i_diff = 0.2 * sol_radiation_dir

    fp_table = [
        [0.25, 0.25, 0.23, 0.19, 0.15, 0.10, 0.06],
        [0.25, 0.25, 0.23, 0.18, 0.15, 0.10, 0.06],
        [0.24, 0.24, 0.22, 0.18, 0.14, 0.10, 0.06],
        [0.22, 0.22, 0.20, 0.17, 0.13, 0.09, 0.06],
        [0.21, 0.21, 0.18, 0.15, 0.12, 0.08, 0.06],
        [0.18, 0.18, 0.17, 0.14, 0.11, 0.08, 0.06],
        [0.17, 0.17, 0.16, 0.13, 0.11, 0.08, 0.06],
        [0.18, 0.18, 0.16, 0.13, 0.11, 0.08, 0.06],
        [0.20, 0.20, 0.18, 0.15, 0.12, 0.08, 0.06],
        [0.22, 0.22, 0.20, 0.16, 0.13, 0.09, 0.06],
        [0.24, 0.24, 0.21, 0.17, 0.13, 0.09, 0.06],
        [0.25, 0.25, 0.22, 0.18, 0.14, 0.09, 0.06],
        [0.25, 0.25, 0.22, 0.18, 0.14, 0.09, 0.06],
    ]
    if posture == "seated":
        fp_table = [
            [0.20, 0.23, 0.21, 0.21, 0.18, 0.16, 0.12],
            [0.20, 0.23, 0.20, 0.20, 0.19, 0.16, 0.12],
            [0.20, 0.23, 0.21, 0.20, 0.18, 0.15, 0.12],
            [0.19, 0.23, 0.20, 0.20, 0.18, 0.15, 0.12],
            [0.18, 0.21, 0.19, 0.19, 0.17, 0.14, 0.12],
            [0.16, 0.20, 0.18, 0.18, 0.16, 0.13, 0.12],
            [0.15, 0.18, 0.17, 0.17, 0.15, 0.13, 0.12],
            [0.16, 0.18, 0.16, 0.16, 0.14, 0.13, 0.12],
            [0.18, 0.18, 0.16, 0.14, 0.14, 0.12, 0.12],
            [0.19, 0.18, 0.15, 0.13, 0.13, 0.12, 0.12],
            [0.21, 0.18, 0.14, 0.12, 0.12, 0.12, 0.12],
            [0.21, 0.17, 0.13, 0.11, 0.11, 0.12, 0.12],
            [0.21, 0.17, 0.12, 0.11, 0.11, 0.11, 0.12],
        ]

    if posture == "supine":
        alt_temp = sol_altitude
        sol_altitude = abs(90 - sol_azimuth)
        sol_azimuth = alt_temp

    alt_range = [0, 15, 30, 45, 60, 75, 90]
    az_range = [0, 15, 30, 45, 60, 75, 90, 105, 120, 135, 150, 165, 180]
    alt_i = find_span(alt_range, sol_altitude)
    az_i = find_span(az_range, sol_azimuth)
    fp11 = fp_table[az_i][alt_i]
    fp12 = fp_table[az_i][alt_i + 1]
    fp21 = fp_table[az_i + 1][alt_i]
    fp22 = fp_table[az_i + 1][alt_i + 1]
    az1 = az_range[az_i]
    az2 = az_range[az_i + 1]
    alt1 = alt_range[alt_i]
    alt2 = alt_range[alt_i + 1]
    fp = fp11 * (az2 - sol_azimuth) * (alt2 - sol_altitude)
    fp += fp21 * (sol_azimuth - az1) * (alt2 - sol_altitude)
    fp += fp12 * (az2 - sol_azimuth) * (sol_altitude - alt1)
    fp += fp22 * (sol_azimuth - az1) * (sol_altitude - alt1)
    fp /= (az2 - az1) * (alt2 - alt1)

    f_eff = 0.725
    if posture == "seated":
        f_eff = 0.696

    sw_abs = asw
    lw_abs = 0.95

    e_diff = f_eff * f_svv * 0.5 * sol_transmittance * i_diff
    e_direct = fp * sol_transmittance * f_bes * sol_radiation_dir
    e_refl = (
        f_eff
        * f_svv
        * 0.5
        * sol_transmittance
        * (sol_radiation_dir * math.sin(sol_altitude * deg_to_rad) + i_diff)
        * floor_reflectance
    )

    e_solar = e_diff + e_direct + e_refl
    erf = e_solar * (sw_abs / lw_abs)
    d_mrt = erf / (hr * f_eff)

    # print(fp, e_diff, e_direct, e_refl, e_solar, erf, d_mrt)

    return {"erf": round(erf, 1), "delta_mrt": round(d_mrt, 1)}

