/*
 * Command line to compile and link this. Run from the directory containing this file
 * g++ -std=c++11 -o quantmath_runner quantmath_runner.cpp ../../target/debug/libquantmath.dylib
 */

#include <iostream>
#include <fstream>
#include <vector>
#include <list>
extern "C" {
#include "quantmath.h"
}

// RAII class to encapsulate a handle from QuantMath, and prevent leaks.
class Handle {
    uint64_t myObj;
public:
    Handle(uint64_t obj = 0) : myObj(obj) {}

    Handle(Handle&& that) : myObj(that.myObj) {
        that.myObj = 0;
    }

    Handle(const Handle& that) : myObj(qm_clone(that.myObj)) {}

    const Handle& operator=(const Handle& that) {
        if (this != &that) {
            if (myObj)
                qm_free_handle(myObj);
            if (that.myObj)
                myObj = qm_clone(that.myObj);
            else
                myObj = 0;
        }
        return *this;
    }

    ~Handle() {
        if (myObj)
            qm_free_handle(myObj);
    }

    uint64_t release() {
        uint64_t tmp = myObj;
        myObj = 0;
        return tmp;
    }

    uint64_t get() const {
        return myObj;
    }
};

// RAII class to encapsulate a string from QuantMath, and prevent leaks.
class Str {
    char* myStr;
public:
    Str(char* str = nullptr) : myStr(str) {}
    
    ~Str() {
        if (myStr)
            qm_free_string(myStr);
    }

    char* release() {
        char* tmp = myStr;
        myStr = nullptr;
        return tmp;
    }

    const char* get() const {
        return myStr;
    }

private:
    Str(const Str& that);
    const Str& operator=(const Str& that);
};

int abort_showing_usage(const char* argv0) {
    std::cerr << "usage: " << argv0 << " [files-to-execute]*" << std::endl;
    std::cerr << "sections: Currencies, Instruments, Market Data, Fixings, Pricer, Reports" << std::endl;
    return -1;
}

bool handle_error(uint64_t handle) {
    Str error_string(qm_error_string(handle));
    std::cerr << error_string.get() << std::endl;
    return false;
}

bool process_file(const char* filename) {
    std::ifstream file (filename);
    if (!file.is_open()) {
        std::cerr << "unable to open file " << filename << std::endl;
        return false;
    }

    std::vector<std::string> currencies;
    std::vector<std::string> instruments;
    std::vector<std::string> market_data;
    std::vector<std::string> fixings;
    std::vector<std::string> pricer_factories;
    std::vector<std::string> report_generators;

    // parse the config file
    std::vector<std::string>* files = nullptr;
    std::string line;
    while (std::getline(file, line)) {
        if (line.size() == 0 || line.front() == '#')
            continue;

        if (line.back() == ':') {
            if (line == "Currencies:")
                files = &currencies;
            else if (line == "Instruments:")
                files = &instruments;
            else if (line == "Market Data:")
                files = &market_data;
            else if (line == "Fixings:")
                files = &fixings;
            else if (line == "Pricer:")
                files = &pricer_factories;
            else if (line == "Reports:")
                files = &report_generators;
            else {
                std::cerr << "Unknown section: \"" << line << "\"" << std::endl;
                return false;
            }
        } else {
            files->push_back(line);
        }
    }
    file.close();

    // read in the currencies
    std::cerr << "read currencies:" << std::endl;
    std::list<Handle> currency_objects;
    std::vector<uint64_t> currency_handles;
    for (const auto& currency_file : currencies) {
        std::cerr << currency_file.c_str() << std::endl;
        currency_objects.emplace_back(qm_currency_from_json_file(currency_file.c_str()));
        const uint64_t handle = currency_objects.back().get();
        if (qm_is_error(handle))
            return handle_error(handle);
        currency_handles.push_back(handle);
    }

    // read in the instruments
    std::cerr << "read instruments:" << std::endl;
    std::list<Handle> instrument_objects;
    std::vector<uint64_t> instrument_handles;
    for (const auto& instrument_file : instruments) {
        std::cerr << instrument_file.c_str() << std::endl;
        instrument_objects.emplace_back(qm_instrument_from_json_file(instrument_file.c_str(),
            QmWriteOnce, currency_handles.size(), currency_handles.data(),
            QmWriteOnce, instrument_handles.size(), instrument_handles.data()));
        const uint64_t handle = instrument_objects.back().get();
        if (qm_is_error(handle))
            return handle_error(handle);
        instrument_handles.push_back(handle);
    }

    // read in the market data
    std::cerr << "read market data" << std::endl;
    std::list<Handle> market_data_objects;
    for (const auto& market_data_file : market_data) {
        market_data_objects.emplace_back(qm_market_data_from_json_file(market_data_file.c_str()));
        const uint64_t handle = market_data_objects.back().get();
        if (qm_is_error(handle))
            return handle_error(handle);
    }
    
    // read in the fixings
    std::cerr << "read fixings" << std::endl;
    std::list<Handle> fixing_objects;
    for (const auto& fixings_file : fixings) {
        fixing_objects.emplace_back(qm_fixing_table_from_json_file(fixings_file.c_str()));
        const uint64_t handle = fixing_objects.back().get();
        if (qm_is_error(handle))
            return handle_error(handle);
    }

    // read in the pricer factories
    std::cerr << "read pricers" << std::endl;
    std::list<Handle> pricer_factory_objects;
    for (const auto& pricer_factory_file : pricer_factories) {
        pricer_factory_objects.emplace_back(qm_pricer_factory_from_json_file(pricer_factory_file.c_str()));
        const uint64_t handle = pricer_factory_objects.back().get();
        if (qm_is_error(handle))
            return handle_error(handle);
    }

    // read in the report generators
    std::cerr << "read report generators" << std::endl;
    std::list<Handle> report_generator_objects;
    std::vector<uint64_t> report_generator_handles;
    for (const auto& report_generator_file : report_generators) {
        report_generator_objects.emplace_back(qm_report_generator_from_json_file(report_generator_file.c_str()));
        const uint64_t handle = report_generator_objects.back().get();
        if (qm_is_error(handle))
            return handle_error(handle);
        report_generator_handles.push_back(handle);
    }

    // for now, we require exactly one each of the pricer factories, market data, and fixings
    if (fixing_objects.size() != 1 || market_data_objects.size() != 1 || pricer_factory_objects.size() != 1) {
        std::cerr << "Require exactly one each of the pricer factories, market data, and fixings" << std::endl;
        return false;
    }

    // do the calculation
    std::cerr << "calculate" << std::endl;
    uint64_t reports = qm_calculate(
        pricer_factory_objects.back().get(), 
        instrument_objects.back().get(), 
        fixing_objects.back().get(),
        market_data_objects.back().get(),
        report_generator_handles.size(), report_generator_handles.data());

    std::cerr << "write reports" << std::endl;
    Str report_str(qm_reports_as_json_string(reports));
    std::cout << report_str.get() << std::endl;
    
    return true;
}

int main(int argc, const char** argv) {
    for (int i = 1; i < argc; ++i) {
        if (!process_file(argv[i]))
            return abort_showing_usage(argv[0]);
    }
}