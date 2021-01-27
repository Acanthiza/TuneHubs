
  library("tidyverse")
  library("fs")
  library("lubridate")
  library("fuzzyjoin")


  services <- tribble(
    ~service, ~hub, ~date, ~bike, ~cost, ~notes
    , 0, "black", "2019-10-02", "open", 150.80
    , "BMCR: Seated & sealed Schwalbe Pro One tyres onto Tune TSR22 rims (was surprisingly difficult, messy, and
time consuming). New tyres should be much easier to seat."
    
    , 1 , "red", "2020-10-02", "open", 907.00
    , "BMCR: Serviced the front hub and headset bearings; replaced the worn rear hub and bottom bracket bearings. Replaced the
worn drive train; degreased the chain and applied Squirt lube. Cleaned, greased and tightened the head stem and
handlebar clamps/bolts, seat clamps/bolts, cranks, chain ring/bolts and pedal threads. Checked the spoke tensions;
trued the wheels. Checked the rear derailleur hanger; adjusted the gears. Checked the brake operation (OK, but the
fluid was dead); flushed and bled the brakes; checked the disc rotor and pad wear (OK); adjusted the brakes.
Checked the tyre sealant (OK). The chain feels a little rough on a couple of rear gears, but this should settle with use.
More Squirt lube will need to be applied to get a full coating on the chain (this should also help with the feel through
the pedals)."
    
    , 1, "red", "2021-01-22", "open", 373.90
    , "BMCR: Serviced both rear hubs; replaced the worn pawls and pawls springs in the red hub. Checked the true and spoke
tensions of the red hub wheel set; checked front rim for dents (OK). Checked the rear derailleur hanger alignment and
gear adjustment. Replaced worn rear gravel tyre. Test rode both wheel sets to check for noise - both seem quiet."
    
    , 1,  "black", "2021-01-22", "open", 0
    , "BMCR: Serviced both rear hubs; replaced the worn pawls and pawls springs in the red hub. Checked the true and spoke
tensions of the red hub wheel set; checked front rim for dents (OK). Checked the rear derailleur hanger alignment and
gear adjustment. Replaced worn rear gravel tyre. Test rode both wheel sets to check for noise - both seem quiet. Nige: $ are on the other hubs"
    
    , 1, "red", "2020-05-25", "open", 257.50
    , "BMCR: Serviced Tune (red) rear hub; siliconed left handlebar plug in place (to stop it rattling); replaced worn rear disc rotor
and all brake pads; adjusted & bedded-in brakes; checked gear adjustment."
    
    , 0, "red", "2020-02-20", "open", 257.50
    , "BMCR: Checked hubs & converted front hub to 12mm system (somehow, I forgot to do this while converting the wheels from
the MTB to the gravel format). Checked headset & BB adjustment. Cleaned, greased & tightened the seat
clamps/bolts. Checked the crank & chain ring bolts. Checked spoke tensions; trued & centralised wheels. Checked the
rear derailleur hanger & checked the gear operation. Replaced worn rear brake pads; checked front pads & rotors for
wear; checked brake operation; adjusted brakes & bedded-in rear pads. Swapped tyres front-to-rear and added
sealant."
    
    , 1, "red", "2020-01-20", "open", 188.00
    , "BMCR: Replaced worn BB & rear hub bearings; cleaned, greased & tightened BB cups; serviced rear hub drive mechanism;
checked & adjusted gears."
    
    , 1, "red", "2019-11-20", "open", 22.50
    , "BMCR: Serviced red Tune Kong hub & drive mechanism; checked gear adjustment."
    
    , 0, "red", "2019-10-22", "open", 770.00
    , "BMCR: Removed old tape(s); cleaned, then re-taped FRM front rim; removed dents & trued wheel. Removed
old tape, cleaned then re-taped Stans rear rim; checked rear wheel; seated & sealed tyres (with
supplied Orange Seal). Converted Tune rear hub to 12mm through axle; spaced rotors to match TSR22
offset; checked gears."
    
    , 1, "red", "2019-09-30", "niner", 193.00
    , "BMCR: Serviced Tune rear hub; checked gear & brake adjustment during test ride. Supplied spare brake pads
for the Open & tyre sealant."
    
    , 1, "black", "2019-01-17", "superX", 1375.80
    , "BMCR: fitted the Tune TSR22 disc wheels; transferred the tyres, cassette and rotors from the old wheels; checked the rear derailleur hanger; and adjusted the gears and brakes"
    
    , 1, "black", "2019-08-19", "open", 360.00
    , "BMCR: Serviced hubs; checked headset; replaced worn BB bearings. Checked head stem & handlebar
clamps/bolts, seat clamps/bolts; applied assembly paste to seat post & cleaned dirt from seat tube.
Cleaned, greased & tightened BB, cranks & pedal threads; checked chain ring bolts. Checked spoke
tensions; trued wheels. Added extra zipties to DI2 cable (inside handlebars). Aligned rear derailleur
hanger; updated DI2 system; reset shifter buttons as per instructions; replaced worn chain (+
degreased chain & lubed with Squirt); adjusted gears. Checked brakes (both are bled perfectly);
checked brake pads & rotors. Only rattle I can find now is from the pump. Swapped tyres (front to rear)
& added sealant."
    
    , 1, "black", "2019-04-18", "open", 7295
    , "Nige: Pete built the Open UP using the TSR-22 wheelset with the black Tune hubs. Presumably they had some sort of 'service' at this point"
    
    ) %>%
    dplyr::mutate(notes = gsub("\\s"," ",notes))
  
  hubServices <- services %>%
    dplyr::filter(service == 1) %>%
    dplyr::arrange(hub,date) %>%
    dplyr::group_by(hub) %>%
    dplyr::mutate(service = row_number()
                  , start = ymd(date)
                  , end = lead(start)
                  , cost = cost
                  ) %>%
    dplyr::ungroup()
  
  kms <- dir_ls(regexp = "csv$") %>%
    tibble::enframe(name = NULL, value = "path") %>%
    dplyr::mutate(data = map(path, read_csv)) %>%
    tidyr::unnest(cols = c(data)) %>%
    dplyr::mutate(When = date(ymd_hms(When))
                  , km = `Dist km`/1000
                  , hrs = `Moving Time`/3600
                  ) %>%
    dplyr::mutate(hub = case_when(When > "2019-01-17" & When <= "2019-09-30" & Gear == "a perfect blend" ~ "black"
                                   , When > "2019-09-30" & Gear == "roadie" ~ "black"
                                   , When > "2019-09-30" & Gear == "a perfect blend" ~ "red"
                                   , TRUE ~ "other"
                                   )
                  ) %>%
    fuzzyjoin::fuzzy_left_join(hubServices
                               , by = c("hub" = "hub"
                                        , "When" = "start"
                                        , "When" = "end"
                                        )
                               , match_fun = list(`==`
                                                  , `>=`
                                                  , `<`
                                                  )
                               ) %>%
    dplyr::select(When,Type,Gear,km,hrs
                  , start
                  , end
                  , hub = hub.x
                  , cost
                  , service
                  , notes
                  )
  
  hubSummary <- kms %>%
    dplyr::filter(hub != "other") %>%
    dplyr::group_by(hub,service,start,end,notes) %>%
    dplyr::summarise(kms = sum(km)
                     , hrs = sum(hrs)
                     ) %>%
    dplyr::select(-notes,everything(),notes)

  write_csv(hubSummary,"hubSummary.csv")  
  