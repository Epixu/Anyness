///                                                                           
/// Langulus::Tester                                                          
/// Copyright (c) 2025 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           

/// INTENTIONALLY NOT GUARDED                                                 
/// Include this file once in each test cpp file, after all other headers     
#ifdef TWOBLUECUBES_SINGLE_INCLUDE_CATCH_HPP_INCLUDED
   #error Catch has already been included prior to this header
#endif

#include <Langulus/Core.hpp>
#include <Langulus/Except.hpp>
#include <cstdint>
#include <cstddef>
#include <vector>
#include <fmt/format.h>

using uint = unsigned int;
using namespace Langulus;

#if LANGULUS(BENCHMARK)
   #define CATCH_CONFIG_ENABLE_BENCHMARKING
#endif


#include <catch2/catch.hpp>

#if LANGULUS(BENCHMARK)
   using timer = Catch::Benchmark::Chronometer;

   template<class T>
   using uninitialized = Catch::Benchmark::storage_for<T>;

   template<class T>
   using some = std::vector<T>;
#endif

/// See https://github.com/catchorg/Catch2/blob/devel/docs/tostring.md        
CATCH_TRANSLATE_EXCEPTION(::Langulus::Exception const& ex) {
   return fmt::format("{}", ex);
}


namespace Catch
{

   #ifdef LANGULUS_LIBRARY_ANYNESS

      /// Save catch2 from doing infinite recursions with Block types         
      template<CT::Block T>
      struct is_range<T> {
         static const bool value = false;
      };

      template<class T>
      concept StringifiableButNotRange = CT::Stringifiable<T> and not Catch::is_range<T>::value;

      template<StringifiableButNotRange T>
      struct StringMaker<T> {
         static std::string convert(T const& value) {
            return ::std::string {Token {static_cast<Anyness::Text>(value)}};
         }
      };

   #endif

   template<>
   struct StringMaker<char8_t> {
      static std::string convert(char8_t const& value) {
         return std::to_string(static_cast<int>(value));
      }
   };

   template<>
   struct StringMaker<char16_t> {
      static std::string convert(char16_t const& value) {
         return std::to_string(static_cast<int>(value));
      }
   };

   template<>
   struct StringMaker<wchar_t> {
      static std::string convert(wchar_t const& value) {
         return std::to_string(static_cast<int>(value));
      }
   };

   /*template<>
   struct StringMaker<::Langulus::Byte> {
      static std::string convert(::Langulus::Byte const& value) {
         return std::to_string(static_cast<int>(value.mValue));
      }
   };*/

}

#ifdef LANGULUS_LIBRARY_LOGGER
/// Dump parse results and requirements                                       
template<class INPUT, class OUTPUT, class REQUIRED>
void DumpResults(const INPUT& in, const OUTPUT& out, const REQUIRED& required) {
   Logger::Special("-------------");
   Logger::Special("Script:   ", in);
   Logger::Special("Parsed:   ", out);
   Logger::Special("Required: ", required);
   Logger::Special("-------------");
}
#endif

#define UNSIGNED_TYPES        ::std::uint8_t, ::std::uint16_t, ::std::uint32_t, ::std::uint64_t
#define REAL_TYPES            Float, Double
#define SIGNED_INTEGER_TYPES  ::std::int8_t, ::std::int16_t, ::std::int32_t, ::std::int64_t
#define INTEGER_TYPES         UNSIGNED_TYPES, SIGNED_INTEGER_TYPES
#define SIGNED_TYPES          SIGNED_INTEGER_TYPES, REAL_TYPES
#define ALL_TYPES             UNSIGNED_TYPES, SIGNED_TYPES


/// Just a bank container, used to contain owned items                        
extern Anyness::TMany<Anyness::Many> BANK;

/// Create a dense element, on the stack                                      
///   @tparam T - type of element we're creating                              
///   @param e - the data we'll use to initialize an instance of T            
///   @return the new instance of T                                           
template<CT::Dense T, bool = false>
T CreateElement(const auto& e) {
   T element;
   if constexpr (CT::Same<T, decltype(e)>)
      element = e;
   else if constexpr (not CT::Same<T, Anyness::Many>)
      element = Decay<T> {e};
   else {
      element = Anyness::Many {};
      element.Insert(e);
   }
   return element;
}

/// Create a sparse element, on the heap                                      
///   @tparam T - type of element we're creating                              
///   @tparam MANAGED - whether we'll have authority over the pointer or not  
///   @param e - the data we'll use to initialize an instance of T            
///   @return pointer to the new instance of T                                
template<CT::Sparse T, bool MANAGED = false>
T CreateElement(const auto& e) {
   T element;

   if constexpr (not MANAGED) {
      // Create a pointer that is guaranteed to not be owned by the     
      // memory manager. Notice we don't use 'new' operator here,       
      // because it is weakly linked, and can be overriden to use our   
      // memory manager.                                                
      if constexpr (not CT::Same<T, Anyness::Many>) {
         element = malloc(sizeof(Deptr<T>));
         new (element) Deptr<T> {e};
      }
      else {
         element = malloc(sizeof(Anyness::Many));
         new (element) Anyness::Many {};
         element->Insert(e);
      }
   }
   else {
      // Create a pointer owned by the memory manager                   
      auto& container = BANK.EmplaceAt(Anyness::Index::Back);

      if constexpr (not CT::Same<T, Anyness::Many>) {
         container << Deptr<T> {e};
         element = container.GetRaw();
      }
      else {
         container << e;
         element = &container;
      }
   }

   return element;
}

template<bool MANAGED = false>
void DestroyElement(auto& e) {
   using E = Deref<decltype(e)>;
   if constexpr (not MANAGED) {
      if constexpr (CT::Sparse<E>) {
         if constexpr (CT::Referenced<Deptr<E>>)
            e->Reference(-1);

         if constexpr (CT::Destroyable<Decay<E>>)
            e->~Decay<E>();

         if constexpr (not MANAGED)
            free(e);
      }
      else if constexpr (requires { DecvqCast(e).Reset(); })
         DecvqCast(e).Reset();
   }
   else BANK.Reset();
}

/// Simple type for testing Referenced types                                  
struct RT : Referenced {
   int data;
   const char* t;
   bool destroyed = false;
   bool copied_in = false;
   bool cloned_in = false;
   bool moved_in = false;
   bool moved_out = false;

   RT()
      : data {0}, t {nullptr} {}

   RT(int a)
      : data {a}, t {nullptr} {}

   RT(const char* tt)
      : data(0), t {tt} {}

   RT(const RT& rhs)
      : data(rhs.data), t {rhs.t}, copied_in {true} {}

   RT(RT&& rhs)
      : data(rhs.data), t {rhs.t}, moved_in {true} {
      rhs.moved_in = false;
      rhs.moved_out = true;
   }

   RT(Cloned<RT>&& rhs)
      : data(rhs->data), t {rhs->t}, cloned_in {true} {
   }

   ~RT() {
      destroyed = true;

      if (GetReferences() == 1)
         Reference(-1);
   }

   RT& operator = (const RT& rhs) {
      data = rhs.data;
      t = rhs.t;
      copied_in = true;
      moved_in = moved_out = false;
      return *this;
   }

   RT& operator = (RT&& rhs) {
      data = rhs.data;
      t = rhs.t;
      copied_in = false;
      moved_in = true;
      moved_out = false;
      rhs.copied_in = false;
      rhs.moved_in = false;
      rhs.moved_out = true;
      return *this;
   }

   operator const int& () const noexcept {
      return data;
   }
};
