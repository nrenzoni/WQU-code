# Continuous Time Stochastic Processes
# Group work submission 2

# assignment:
# A pension fund bought OTC long expiry put options on S&P 500 index at a strike of 3000
# expiring in December 2020. However, they are unsure about the position and were thinking
# of either selling a Put or a Call on their position. Price both the options for them and make a
# recommendation based on the price you derive.

import numpy as np
import scipy.stats as si
import pandas as pd
import datetime as dt


def calc_discounted_time(r, T):
    return np.exp(-r * T)


def calc_d1(S, K, T, r, sigma):
    return (np.log(S / K) + (r + 0.5 * sigma ** 2) * T) / (sigma * np.sqrt(T))


def calc_d2(S, K, T, r, sigma):
    return (np.log(S / K) + (r - 0.5 * sigma ** 2) * T) / (sigma * np.sqrt(T))


# source: https://aaronschlegel.me/black-scholes-formula-python.html
def calc_euro_call_option(S, K, T, r, sigma):
    """
    :param S: spot price
    :param K: strike price
    :param T: time to maturity
    :param r: interest rate
    :param sigma: volatility of underlying asset
    :return: price of european-style call option
    """

    d1 = calc_d1(S, K, T, r, sigma)
    d2 = calc_d2(S, K, T, r, sigma)

    return S * si.norm.cdf(d1, 0.0, 1.0) \
           - K * calc_discounted_time(r, T) * si.norm.cdf(d2, 0.0, 1.0)


# source: https://aaronschlegel.me/black-scholes-formula-python.html
def calc_euro_put_option(S, K, T, r, sigma):
    """
    :param S: spot price
    :param K: strike price
    :param T: time to maturity
    :param r: interest rate
    :param sigma: volatility of underlying asset
    :return: price of european-style put option
    """

    d1 = calc_d1(S, K, T, r, sigma)
    d2 = calc_d2(S, K, T, r, sigma)

    return K * calc_discounted_time(r, T) * si.norm.cdf(-d2, 0.0, 1.0) \
           - S * si.norm.cdf(-d1, 0.0, 1.0)


if __name__ == '__main__':
    # read sp500 price series
    sp500_price_series = pd.read_excel(
        "20200701_Index Price and Volume Data.xlsx",
        header=4,
        index_col=0,
        usecols=[0, 1],
        sheet_name=1,
        squeeze=True,
        skiprows=2,
        parse_dates=True
    )

    yearly_std = 0.30

    option_underlying_price = sp500_price_series[-1]

    otc_option_purchase_date = sp500_price_series.index[-1].date()  # 6/30/2020
    otc_option_exp = dt.date(2020, 12, 15)

    # ~ 3 months
    trading_days_remaining_until_exp = np.busday_count(
        otc_option_purchase_date,
        otc_option_exp
    )

    normalized_days_remaining = trading_days_remaining_until_exp / 255

    # assumption
    interest_rate = 0.005

    strike_price = 3_000

    price_of_call_option = calc_euro_call_option(
        S=option_underlying_price,
        K=strike_price,
        T=normalized_days_remaining,
        r=interest_rate,
        sigma=yearly_std
    )

    price_of_put_option = calc_euro_put_option(
        S=option_underlying_price,
        K=strike_price,
        T=normalized_days_remaining,
        r=interest_rate,
        sigma=yearly_std
    )

    print(f"price of call option: {price_of_call_option}\n"
          f"price of put option: {price_of_put_option}")
