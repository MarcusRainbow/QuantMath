# QuantMath
Financial maths library for risk-neutral pricing and risk

[Api Documentation](https://docs.rs/quantmath/0.1.0/quantmath/)

## Goals
Some quant math libraries are really just a collection of pricing formulae. This hopes to be that (see the math module) but also much more. This library is intended to be plugged into the risk and pricing infrastructure in an investment bank or hedge fund. This does not preclude the use of the library for academic work, but will introduce a level of real-world messiness that is often missing from academia.

### Lifecycle and Flows
QuantMath is responsible for managing the lifecycle of financial instruments (see the instrument module). As products make payments or as dividends go ex, this results in the instrument splitting into multiple flows. Nothing ever disappears. The SecDB library at Goldman Sachs is famous for taking this philosophy to extremes, but QuantMath is at least capable of the same level of discipline. It is vital to correctly model when flows go ex -- when they cease to be part of the value of this instrument, and are owned by the counterparty (even if not yet settled).

### Settlement
Most investment banks skirt around the issues of settlement. What is the value of an equity? Is it the spot price, or is it the spot price discounted from the date when the payment for the equity would actually be received (e.g. T+2). QuantMath intends to allow rigour in settlement, in the premium payment, the payoff, and in the underlying hedges.

### Risk and Scenarios
QuantMath is designed to make it easy to reuse calculations that have not changed as a result of a risk bump. For example, if you have an exotic product with multiple equity underlyings, bumping one of those underlyings only results in the affected Monte-Carlo paths being reevaluated. In my experience this is a vital optimisation, and QuantMath makes it possible from the lowest levels such as bootstrapping dividend curves, to the highest, such as reuse of Longstaff-Schwarz optimisation.

### Recursive Instruments
It is common to build instruments recursively. A basket contains composite or quanto underliers, then a dynamic index maintains the basket -- finally an exotic product is written with the dynamic index as an underlying. The library must therefore manage this sort of recursive product, whether valuing in Monte-Carlo, analytically or via a finite difference engine.

### Simplicity, Orthogonality and Encapsulation
The library must be easy for quants to work in and for IT systems to work with. Adding a new risk, instrument or model should normally mean changes to only one file (and maybe a list of files in mod.rs). The interface to IT should be data-driven, so IT do not need to rebuild every time an instrument or model is added. Models, instruments and risks should be orthogonal, so any can be used with any (subject to sensible mathematical restrictions). If things go wrong, it should be easy to debug just QuantMath, without having to debug the containing IT system. This means that QuantMath should be runnable purely from serialised state, such as JSON files.

## The Architecture
The library has a strict hierarchy of modules. Ideally there should be no backward dependencies, such that the library could be split into a separate crate for each module. If you are looking at the library for the first time, it may be best to start from the top level (Facade). Starting at the top level, the modules are:

### Facade
This is the interface that IT systems talk to. It is data-driven, so adding a new product or model should not affect the IT systems at all.

### Pricers
A pricer evaluates an instrument given market data and a choice of model. We currently have two pricers: Monte-Carlo, which evaluates instruments by averaging across many random paths; self-pricer, which relies on instruments knowing how to price themselves. I hope to add at least one finite difference backward-induction engine.

### Models
Models of how we expect prices to change in the future. All are stochastic, but some have stochastic volatility or rates. Examples of models are BGM (Brace Gatarek Musiela), Black, Heston.

### Risk
Defines how market data can be bumped, and manages the dependencies when this happens. This contain definitions of risk reports, such as Delta, Gamma, Vega and Volga for all underliers matching some criteria.

### Instruments
Defines financial products, indices, assets and currencies. Anything that has a price. Some instruments know how to price themselves (basically, any instrument where the price is well-defined and not model-dependent -- remember this module is lower than models). Some instruments know how to price themselves in a Monte-Carlo framework, given paths of their underliers.

### Data
The input market data; vol surfaces, dividends, spot prices, yield curves etc. Also defines bumps to these data items. Most risks are calculated by bumping these inputs.

### Math
Low level mathematical formulae, from the Black-Scholes formula to interpolation and quadrature. Where possible, we use functionality from well-established crates in Rust, such as ndarray and statrs, so this is mainly quant-specific maths.

### Dates
Dates are very important for financial maths software. We use explicit dates everywhere rather than year-fractions, which is essential for handling settlement correctly. This module also handles date arithmetic, such as date rules and day counts.

### Core
Very low-level functionality, such as the definition of the Error struct, and required extensions to serde, such as dedup (deduplication of nodes in directed acyclic graph) and factories (using tagged_serde to handle polymorphic nodes in serialization and deserialization).
