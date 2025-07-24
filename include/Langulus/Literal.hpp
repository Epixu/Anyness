///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Core.hpp"
#include <array>
#include <functional>
#include <iterator>
#include <string_view>

#if LANGULUS(SAFE)
   #include <stdexcept>
#endif


namespace Langulus
{
   namespace CT
   {
      /// Check if all T are Literal types                                    
      template<class...T>
      concept Literal = Inner::CheckSize<T...>() and (T::CTTI_Literal and ...);
      
      /// Supported character types used by LiteralString                     
      template<class...T>
      concept LiteralChar = Inner::CheckSize<T...>() and ((
              ::std::same_as<::std::remove_cv_t<T>, char>
           or ::std::same_as<::std::remove_cv_t<T>, wchar_t>
           or ::std::same_as<::std::remove_cv_t<T>, char8_t>
           or ::std::same_as<::std::remove_cv_t<T>, char16_t>
           or ::std::same_as<::std::remove_cv_t<T>, char32_t>
         ) and ...);
      
      /// Check if all T are Literal strings                                  
      template<class...T>
      concept LiteralString = Literal<T...>
          and ((T::ArraySize > 0 and LiteralChar<typename T::value_type>) and ...);
      
      /// Check if all T are Literal values                                   
      template<class...T>
      concept LiteralValue = Literal<T...> and ((T::ArraySize == 0
          and not ::std::same_as<::std::remove_cv_t<typename T::value_type>, Unsupported>) and ...);
      
      /// Check if all T are Literal values, but undefined                    
      template<class...T>
      concept LiteralUndefined = Literal<T...>
          and (::std::same_as<::std::remove_cv_t<typename T::value_type>, Unsupported> and ...);
      
   }

   using Token = ::std::string_view;


   ///                                                                        
   /// Acts as both a single value, or string literal                         
   /// You can use it as a template parameter                                 
   /// The string implementation should be introduced in C++26 as             
   /// std::fixed_string, supposedly...                                       
   ///                                                                        
   /// String literals are unique types, they can't be used in ?: statements, 
   /// so I've allowed string literals of the form `? "\0\0\0" : "alt"` to    
   /// be consistent - left literal has a Literal array size of 3, but size() 
   /// of 0                                                                   
   ///                                                                        
   template<class T, size_t N>
   struct Literal {
      static constexpr bool CTTI_Literal = true;
      static constexpr bool Undefined = ::std::same_as<T, Unsupported>;
      static constexpr size_t ArraySize = N;

      using storage_type = ::std::array<T, N + 1>;
      storage_type _data {};

      using value_type = T;
      using pointer = value_type*;
      using const_pointer = const value_type*;
      using reference = value_type&;
      using const_reference = const value_type&;
      using iterator = typename storage_type::iterator;
      using const_iterator = typename storage_type::const_iterator;
      using reverse_iterator = typename storage_type::reverse_iterator;
      using const_reverse_iterator = typename storage_type::const_reverse_iterator;
      using difference_type = ptrdiff_t;
      using view_type = ::std::basic_string_view<value_type>;

      static constexpr size_t npos = view_type::npos;

      constexpr Literal() noexcept = default;

      constexpr Literal(const value_type& c) noexcept {
         _data[0] = c;
      }

      template<size_t M> requires (M <= N)
      constexpr Literal(const Literal<char, M>& other) noexcept {
         for (size_t i = 0; i < M; i++)
            _data[i] = other._data[i];
         _data[M] = 0;
      }

      constexpr Literal(const value_type(&array)[N]) noexcept {
         ::std::copy(::std::begin(array), ::std::end(array), _data.begin());
      }

      constexpr Literal& operator = (const value_type(&array)[N]) noexcept {
         ::std::copy(::std::begin(array), ::std::end(array), _data.begin());
         return *this;
      }

      ///                                                                     
      /// Iteration                                                           
      ///                                                                     
      constexpr auto begin(this auto&& self) noexcept {
         return self._data.begin();
      }

      constexpr auto end(this auto&& self) noexcept {
         return self._data.begin() + self.size();
      }

      constexpr auto cbegin() const noexcept {
         return _data.cbegin();
      }

      constexpr auto cend(this auto&& self) noexcept {
         return self._data.cbegin() + self.size();
      }

      constexpr auto rbegin(this auto&& self) noexcept {
         return self._data.rbegin() + (N - self.size());
      }

      constexpr auto rend(this auto&& self) noexcept {
         return self._data.rend();
      }

      constexpr auto crbegin(this auto&& self) noexcept {
         return self._data.crbegin() + (N - self.size());
      }

      constexpr auto crend() const noexcept {
         return _data.crend();
      }

      ///                                                                     
      /// Encapsulation                                                       
      ///                                                                     
      constexpr size_t size() const noexcept {
         if constexpr (N > 0 and not Undefined) {
            // This is a slow implementation, but Literals are mostly   
            // used at compile-time, so it shouldn't be an issue        
            auto ptr = _data.data();
            const auto ptrEnd = _data.data() + N;
            while(ptr != ptrEnd and *ptr)
               ++ptr;
            return ptr - _data.data();
         }
         else return 0;
      }
      
      constexpr bool empty() const noexcept {
         if constexpr (N > 0 and not Undefined)
            return not N or not _data[0];
         else
            return true;
      }
      
      constexpr explicit operator bool () const noexcept {
         if constexpr (Undefined) return false;
         else return _data[0];
      }

      ///                                                                     
      /// Access                                                              
      /// @attention 'n' is always 0 when N == 0                              
      constexpr decltype(auto) operator [] (this auto&& self, [[maybe_unused]] size_t n)
      has_assumptions {
         if constexpr (N > 0) {
            #if LANGULUS_SAFE()
               if not consteval {
                  if (n >= self.size()) throw ::std::range_error(HERE());
               }
            #endif
            return self._data[n];
         }
         else return self._data[0];
      }

      constexpr decltype(auto) at(this auto&& self, size_t n) {
         return self._data.at(n);
      }

      constexpr decltype(auto) front(this auto&& self) noexcept {
         return self._data.front();
      }

      constexpr decltype(auto) back(this auto&& self) noexcept {
         return self._data[self.size() - 1];
      }

      constexpr auto data(this auto&& self) noexcept {
         return self._data.data();
      }

      constexpr auto c_str() const noexcept {
         return _data.data();
      }

      ///                                                                     
      /// Retype                                                              
      ///                                                                     
      /// Get a resized Literal with the same properties                      
      template<size_t M>
      using Resized = Literal<value_type, M>;

   protected:
      template<class, size_t>
      friend struct Literal;

      template<size_t pos, size_t count, size_t size>
      consteval static size_t clamp() {
         if constexpr (pos >= size)
            return 0;
         return count < size - pos ? count : size - pos;
      }

      constexpr view_type sv() const { return *this; }

   public:
      /// Implicit cast to a first value, if N == 0                           
      constexpr operator T() const noexcept requires (N == 0) {
         return _data[0];
      }
      
      /// Implicit cast to a string view, if N > 0                            
      constexpr operator view_type() const noexcept requires (N > 0) {
         return {data(), size()};
      }

      /// Get a region of the string                                          
      template<size_t pos = 0, size_t count = npos> requires (pos <= N)
      constexpr auto substr() const noexcept {
         using Selection = Resized<clamp<pos, count, N>()>;
         Selection result;
         std::copy(begin() + pos, begin() + pos + Selection::ArraySize, result.begin());
         return result;
      }

      /// Find                                                                
      template <size_t M>
      constexpr size_t find(const Resized<M>& str, size_t pos = 0) const noexcept {
         if constexpr (M > N)
            return npos;
         return sv().find(str.sv(), pos);
      }
      constexpr size_t find(const view_type& view, size_t pos = 0) const noexcept {
         return sv().find(view, pos);
      }
      constexpr size_t find(const value_type* s, size_t pos, size_t n) const {
         return sv().find(s, pos, n);
      }
      constexpr size_t find(const value_type* s, size_t pos = 0) const {
         return sv().find(s, pos);
      }
      constexpr size_t find(value_type c, size_t pos = 0) const noexcept {
         return sv().find(c, pos);
      }

      /// Find in reverse                                                     
      template <size_t M>
      constexpr size_t rfind(const Resized<M>& str, size_t pos = npos) const noexcept {
         if constexpr (M > N)
            return npos;
         return sv().rfind(str.sv(), pos);
      }
      constexpr size_t rfind(const view_type& view, size_t pos = npos) const noexcept {
         return sv().rfind(view, pos);
      }
      constexpr size_t rfind(const value_type* s, size_t pos, size_t n) const {
         return sv().rfind(s, pos, n);
      }
      constexpr size_t rfind(const value_type* s, size_t pos = npos) const {
         return sv().rfind(s, pos);
      }
      constexpr size_t rfind(value_type c, size_t pos = npos) const noexcept {
         return sv().rfind(c, pos);
      }

      /// Find the first of                                                   
      template <size_t M>
      constexpr size_t find_first_of(const Resized<M>& str, size_t pos = 0) const noexcept {
         if constexpr (M > N)
            return npos;
         return sv().find_first_of(str.sv(), pos);
      }
      constexpr size_t find_first_of(const view_type& view, size_t pos = 0) const noexcept {
         return sv().find_first_of(view, pos);
      }
      constexpr size_t find_first_of(const value_type* s, size_t pos, size_t n) const {
         return sv().find_first_of(s, pos, n);
      }
      constexpr size_t find_first_of(const value_type* s, size_t pos = 0) const {
         return sv().find_first_of(s, pos);
      }
      constexpr size_t find_first_of(value_type c, size_t pos = 0) const noexcept {
         return sv().find_first_of(c, pos);
      }

      /// Find the last of                                                    
      template <size_t M>
      constexpr size_t find_last_of(const Resized<M>& str, size_t pos = npos) const noexcept {
         if constexpr (M > N)
            return npos;
         return sv().find_last_of(str.sv(), pos);
      }
      constexpr size_t find_last_of(const view_type& view, size_t pos = npos) const noexcept {
         return sv().find_last_of(view, pos);
      }
      constexpr size_t find_last_of(const value_type* s, size_t pos, size_t n) const {
         return sv().find_last_of(s, pos, n);
      }
      constexpr size_t find_last_of(const value_type* s, size_t pos = npos) const {
         return sv().find_last_of(s, pos);
      }
      constexpr size_t find_last_of(value_type c, size_t pos = npos) const noexcept {
         return sv().find_last_of(c, pos);
      }

      /// Find the first NOT of                                               
      template <size_t M>
      constexpr size_t find_first_not_of(const Resized<M>& str, size_t pos = 0) const noexcept {
         if constexpr (M > N)
            return npos;
         return sv().find_first_not_of(str.sv(), pos);
      }
      constexpr size_t find_first_not_of(const view_type& view, size_t pos = 0) const noexcept {
         return sv().find_first_not_of(view, pos);
      }
      constexpr size_t find_first_not_of(const value_type* s, size_t pos, size_t n) const {
         return sv().find_first_not_of(s, pos, n);
      }
      constexpr size_t find_first_not_of(const value_type* s, size_t pos = 0) const {
         return sv().find_first_not_of(s, pos);
      }
      constexpr size_t find_first_not_of(value_type c, size_t pos = 0) const noexcept {
         return sv().find_first_not_of(c, pos);
      }

      /// Find the last NOT of                                                
      template <size_t M>
      constexpr size_t find_last_not_of(const Resized<M>& str, size_t pos = npos) const noexcept {
         if constexpr (M > N)
            return npos;
         return sv().find_last_not_of(str.sv(), pos);
      }
      constexpr size_t find_last_not_of(const view_type& view, size_t pos = npos) const noexcept {
         return sv().find_last_not_of(view, pos);
      }
      constexpr size_t find_last_not_of(const value_type* s, size_t pos, size_t n) const {
         return sv().find_last_not_of(s, pos, n);
      }
      constexpr size_t find_last_not_of(const value_type* s, size_t pos = npos) const {
         return sv().find_last_not_of(s, pos);
      }
      constexpr size_t find_last_not_of(value_type c, size_t pos = npos) const noexcept {
         return sv().find_last_not_of(c, pos);
      }

      /// Compare                                                             
      constexpr int compare(view_type v) const noexcept {
         return sv().compare(v);
      }
      constexpr int compare(size_t pos1, size_t count1, view_type v) const {
         return sv().compare(pos1, count1, v);
      }
      constexpr int compare(size_t pos1, size_t count1, view_type v, size_t pos2, size_t count2) const {
         return sv().compare(pos1, count1, v, pos2, count2);
      }
      constexpr int compare(const value_type* s) const {
         return sv().compare(s);
      }
      constexpr int compare(size_t pos1, size_t count1, const value_type* s) const {
         return sv().compare(pos1, count1, s);
      }
      constexpr int compare(size_t pos1, size_t count1, const value_type* s, size_t count2) const {
         return sv().compare(pos1, count1, s, count2);
      }

      /// Starts with                                                         
      constexpr bool starts_with(view_type v) const noexcept {
         return sv().substr(0, v.size()) == v;
      }
      constexpr bool starts_with(char c) const noexcept {
         return not empty() and ::std::char_traits<T>::eq(front(), c);
      }
      constexpr bool starts_with(const value_type* s) const noexcept {
         return starts_with(view_type(s));
      }

      /// Ends with                                                           
      constexpr bool ends_with(view_type sv) const noexcept {
         return size() >= sv.size() && compare(size() - sv.size(), npos, sv) == 0;
      }
      constexpr bool ends_with(value_type c) const noexcept {
         return !empty() && ::std::char_traits<T>::eq(back(), c);
      }
      constexpr bool ends_with(const value_type* s) const {
         return ends_with(view_type(s));
      }

      /// Contains                                                            
      constexpr bool contains(view_type sv) const noexcept {
         return find(sv) != npos;
      }
      constexpr bool contains(value_type c) const noexcept {
         return find(c) != npos;
      }
      constexpr bool contains(const value_type* s) const {
         return find(s) != npos;
      }

      void swap(Literal& other) noexcept(std::is_nothrow_swappable_v<storage_type>) {
         _data.swap(other._data);
      }
   };

   Literal() -> Literal<Unsupported, 0>;

   template<class T>
   Literal(const T&) -> Literal<T, 0>;
   
   template<class T, size_t N>
   Literal(const T(&)[N]) -> Literal<T, N>;


   /// Swap two strings                                                       
   template<CT::LiteralString S>
   void swap(S& lhs, S& rhs) noexcept(noexcept(lhs.swap(rhs))) {
      lhs.swap(rhs);
   }


   ///                                                                        
   /// Literal == Literal                                                     
   template<CT::Literal LHS, CT::Literal RHS>
   constexpr bool operator == (const LHS& lhs, const RHS& rhs) {
      if constexpr (CT::LiteralString<LHS, RHS>) {
         // Both are strings                                            
         if (lhs.size() != rhs.size())
            return false;
      
         for (size_t i = 0; i < lhs.size(); ++i) {
            if (lhs[i] != rhs[i])
               return false;
         }
         return true;
      }
      else if constexpr (CT::LiteralString<LHS>) {
         // LHS is string, RHS is value/undefined                       
         if constexpr (CT::LiteralUndefined<RHS>)
            return lhs.empty();
         else if constexpr (::std::equality_comparable_with<typename LHS::value_type, typename RHS::value_type>)
            return (lhs.empty() and rhs.empty()) or (lhs.size() == 1 and lhs[0] == rhs[0]);
         else
            return false;
      }
      else if constexpr (CT::LiteralString<RHS>) {
         // LHS is value/undefined, RHS is string                       
         if constexpr (CT::LiteralUndefined<LHS>)
            return rhs.empty();
         else if constexpr (::std::equality_comparable_with<typename LHS::value_type, typename RHS::value_type>)
            return (lhs.empty() and rhs.empty()) or (rhs.size() == 1 and lhs[0] == rhs[0]);
         else
            return false;
      }
      else if constexpr (::std::equality_comparable_with<typename LHS::value_type, typename RHS::value_type>) {
         // Both are values/undefined and comparable                    
         return lhs[0] == rhs[0];
      }
      else {
         // Both are values/undefined and uncomparable, and can be the  
         // same only if both are undefined                             
         return CT::LiteralUndefined<LHS, RHS>;
      }
   }

   /// Literal == View                                                        
   template<CT::LiteralString S>
   constexpr bool operator == (const S& lhs, typename S::view_type rhs) {
      return static_cast<typename S::view_type>(lhs) == rhs;
   }

   /// View == Literal                                                        
   template<CT::LiteralString S>
   constexpr bool operator == (typename S::view_type lhs, const S& rhs) {
      return lhs == static_cast<typename S::view_type>(rhs);
   }

   /// View == Undefined                                                      
   template<CT::LiteralUndefined S>
   constexpr bool operator == (const ::std::string_view& lhs, const S&) {
      return lhs.empty();
   }

   /// Literal == Array                                                       
   template<CT::LiteralString S, size_t N>
   constexpr bool operator == (const S& lhs, const typename S::value_type(&rhs)[N]) {
      return lhs == static_cast<typename S::view_type>(rhs);
   }

   /// Array == Literal                                                       
   template<CT::LiteralString S, size_t N>
   constexpr bool operator == (const typename S::value_type(&lhs)[N], const S& rhs) {
      return static_cast<typename S::view_type>(lhs) == rhs;
   }

   /// LiteralValue == Array                                                  
   template<CT::LiteralValue S, size_t N>
   constexpr bool operator == (const S& lhs, const typename S::value_type(&rhs)[N]) {
      return lhs[0] == rhs[0];
   }

   /// Array == LiteralValue                                                  
   template<CT::LiteralValue S, size_t N>
   constexpr bool operator == (const typename S::value_type(&lhs)[N], const S& rhs) {
      return lhs[0] == rhs[0];
   }

   /// LiteralUndefined == Array                                              
   template<CT::LiteralUndefined S, CT::LiteralChar C, size_t N>
   constexpr bool operator == (const S&, const C(&rhs)[N]) {
      return rhs[0] == '\0';
   }

   /// Array == LiteralUndefined                                              
   template<CT::LiteralUndefined S, CT::LiteralChar C, size_t N>
   constexpr bool operator == (const C(&lhs)[N], const S&) {
      return lhs[0] == '\0';
   }


   ///                                                                        
   /// Literal <=> Literal                                                    
   constexpr auto operator <=> (
      const CT::LiteralString auto& lhs,
      const CT::LiteralString auto& rhs
   ) {
      using lhs_type = std::decay_t<decltype(lhs)>;
      using sv_type = typename lhs_type::view_type;
      return static_cast<sv_type>(lhs) <=> rhs;
   }

   /// Literal <=> View                                                       
   template<CT::LiteralString S>
   constexpr auto operator <=> (const S& lhs, const typename S::view_type& rhs) {
      return static_cast<typename S::view_type>(lhs) <=> rhs;
   }
   
   /// View <=> Literal                                                       
   template<CT::LiteralString S>
   constexpr auto operator <=> (const typename S::view_type& lhs, const S& rhs) {
      return lhs <=> static_cast<typename S::view_type>(rhs);
   }
   
   /// Literal <=> Array                                                      
   template<CT::LiteralString S, size_t N>
   constexpr auto operator <=> (const S& lhs, const typename S::value_type(&rhs)[N]) {
      using sv_type = typename S::view_type;
      return static_cast<sv_type>(lhs) <=> sv_type {rhs};
   }
   
   /// Array <=> Literal                                                      
   template<CT::LiteralString S, size_t N>
   constexpr auto operator <=> (const typename S::value_type(&lhs)[N], const S& rhs) {
      using sv_type = typename S::view_type;
      return sv_type {lhs} <=> static_cast<sv_type>(rhs);
   }
   

   ///                                                                        
   /// Concatenation                                                          
   ///                                                                        
   template<CT::LiteralString LHS, CT::LiteralString RHS>
   constexpr auto operator + (const LHS& lhs, const RHS& rhs) {
      typename LHS::template Resized<LHS::ArraySize + RHS::ArraySize> result;
      size_t i = 0;
      for (; i < lhs.size(); ++i)
         result[i] = lhs[i];
      for (; i < lhs.size() + rhs.size(); ++i)
         result[i] = rhs[i - lhs.size()];
      return result;
   }

   template<CT::LiteralChar C, size_t N>
   constexpr auto operator + (const C(&lhs)[N], const CT::LiteralString auto& rhs) {
      Literal lhs2 = lhs;
      return lhs2 + rhs;
   }

   template<CT::LiteralChar C, size_t N>
   constexpr auto operator + (const CT::LiteralString auto& lhs, const C(&rhs)[N]) {
      Literal rhs2 = rhs;
      return lhs + rhs2;
   }

   namespace Inner
   {
      template<class T>
      constexpr auto from_char(T ch) {
         Literal<T, 1> fs;
         fs[0] = ch;
         return fs;
      }
   }

   constexpr auto operator + (CT::LiteralChar auto lhs, const CT::LiteralString auto& rhs) {
      return Inner::from_char(lhs) + rhs;
   }

   constexpr auto operator + (const CT::LiteralString auto& lhs, CT::LiteralChar auto rhs) {
      return lhs + Inner::from_char(rhs);
   }

   
   /// Equivalent to ::std::true_type, but without the silly nomenclature     
   /// Can carry a constant with itself                                       
   template<Literal VALUE = 0>
   struct Yes {
      static constexpr auto Constant = VALUE;
      static constexpr bool Enabled = true;
   };

   /// Equivalent to ::std::false_type or ::std::true_type, depending on arg  
   /// Can carry a constant with itself                                       
   template<bool CONDITION, Literal VALUE = 0>
   struct Maybe {
      static constexpr auto Constant = VALUE;
      static constexpr bool Enabled = CONDITION;
   };

   /// Equivalent to ::std::false_type, but without the silly nomenclature    
   struct No {
      using CTTI_Void = Yes<>;
      static constexpr bool Enabled = false;
   };
}

namespace std
{
   ///                                                                        
   /// Hash support                                                           
   ///                                                                        
   template<class TChar, size_t N>
   struct hash<Langulus::Literal<TChar, N>> {
      using argument_type = Langulus::Literal<TChar, N>;

      LANGULUS(INLINED)
      size_t operator()(const argument_type& str) const {
         using sv_t = typename argument_type::string_view_type;
         return hash<sv_t>()(static_cast<sv_t>(str));
      }
   };
}
