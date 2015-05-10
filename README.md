# sneezr
An R wrapper for Claritin's pollen count api

[Claritin's webapp](http://www.claritin.com/allergy-forecast/)

[Claritin's json feed](http://www.claritin.com/weatherpollenservice/weatherpollenservice.svc/getforecast/[zipcode])

## Needs work:
- Figure out a license compatible with Claritin's TOS
- Better organize json parsing
- Reduce dependencies

## Usage
```
df <- pollencast(zip = 5digitzip)
```
