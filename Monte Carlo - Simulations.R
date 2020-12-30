# Sales monthly forecast

sunny_day_prob = 0.8
rainy_day_prob = 1 - sunny_day_prob
mean_unit_sales_sunny_days = 50
mean_unit_sales_rainy_days = 35
sd_unit_sales_sunny_days = 8
sd_unit_sales_rainy_days = 5
unit_produced = 30
profit_rate = 0.60
loss_rate = 0.15

for (i in 1) {
  day = seq(i,30,1)
  random_prob = runif(30,0,1)
  weather = ifelse(random_prob <= rainy_day_prob, "Rain", "Sun")
  demand = round(ifelse(weather == "Rain", 
                        qnorm(p = runif(1, 0, 1), mean = mean_unit_sales_rainy_days, sd = sd_unit_sales_rainy_days),
                        qnorm(p = runif(1, 0, 1), mean = mean_unit_sales_sunny_days, sd = sd_unit_sales_sunny_days)), 0)
  sales = ifelse(demand > unit_produced, unit_produced, demand)
  unsale_units = ifelse(demand > unit_produced, 0,  unit_produced - demand)
  profit = sales * profit_rate
  loss = unsale_units * loss_rate
  results = profit - loss
  simulation = data.frame(day, random_prob, weather, demand, sales, unsale_units, profit, loss, results)
  result = sum(results)
  print(simulation)
  print(paste("Earnings = ",result))
}

# operative option coverage

unit_sale_price = 3500
unit_cost = -600
commodity_spot_price = 1800

long_call_strike = 2000
long_call_premium = -50
short_put_strike = 1700
short_put_premium = 40

spot_price = seq(500,3000,100)
call_cash_flow = ifelse(long_call_strike <= spot_price, -long_call_strike, 0)
put_cash_flow = ifelse(short_put_strike >= spot_price, -short_put_strike, 0)
premiums_cost = rep(short_put_premium + long_call_premium, length(spot_price))
cash_flow = ifelse(call_cash_flow + put_cash_flow + premiums_cost != 0, call_cash_flow + put_cash_flow + premiums_cost, -commodity_spot_price)
call_value = ifelse(long_call_strike < spot_price, spot_price - long_call_strike, 0)
put_value = ifelse(short_put_strike > spot_price, spot_price - short_put_strike, 0)

result_without_coverage = commodity_spot_price - spot_price
result_with_coverage = result_without_coverage + premiums_cost + call_value + put_value

unit_sale_price_vector = rep(unit_sale_price, length(spot_price))
unit_cost_vector = rep(unit_cost, length(spot_price))
yield_with_coverage = unit_sale_price_vector + unit_cost_vector + cash_flow
yield_without_coverage = unit_sale_price_vector + unit_cost_vector - spot_price

plot(y = yield_with_coverage, x =  spot_price, type = "l", ylab = "Cash flow", xlab = "Spot price", ylim = c(-100, max(yield_with_coverage) + 1000))
abline(h = 0, col = "red")

plot(y = yield_without_coverage, x =  spot_price, type = "l", ylab = "Cash flow", xlab = "Spot price", ylim = c(-100, max(yield_without_coverage) + 1000))
abline(h = 0, col = "red")

# financial option strategies

long_call_strike = 100
long_call_premium = -12
long_put_strike = 100
long_put_premium = -12

spot_price = seq(10,300,10)
call_value = ifelse(long_call_strike < spot_price, spot_price - long_call_strike, 0)
put_value = ifelse(long_put_strike > spot_price, long_put_strike - spot_price, 0)
premiums_cost = rep(long_call_premium + long_put_premium, length(spot_price))
yield = call_value + put_value + premiums_cost
plot(y = yield, x = spot_price, type = "l", main = "Long Straddle")
abline(h = 0, col = "red")
