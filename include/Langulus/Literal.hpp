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
   #include <exception>
#endif


namespace Langulus
{
   /// A fully portable constexpr alphabetical character check                
   /// Only english alphabet and underline symbol are allowed                 
   constexpr bool IsAlphabetical(char c) noexcept {
      switch (c) {
      case 'A': case 'a': case 'B': case 'b': case 'C': case 'c':
      case 'D': case 'd': case 'E': case 'e': case 'F': case 'f':
      case 'G': case 'g': case 'H': case 'h': case 'I': case 'i':
      case 'J': case 'j': case 'K': case 'k': case 'L': case 'l':
      case 'M': case 'm': case 'N': case 'n': case 'O': case 'o':
      case 'P': case 'p': case 'Q': case 'q': case 'R': case 'r':
      case 'S': case 's': case 'T': case 't': case 'U': case 'u':
      case 'V': case 'v': case 'W': case 'w': case 'X': case 'x':
      case 'Y': case 'y': case 'Z': case 'z': case '_':
         return true;
      default:
         return false;
      }
   }

   /// A fully portable constexpr operator character check                    
   /// Only operators that can occur in type names are allowed                
   constexpr bool IsOperator(char c) noexcept {
      switch (c) {
      case '<': case '>': case '[': case ']': case '(': case ')':
      case '*': case '&': case ':': case ';': case '"': case '\'':
      case '.': case ',':
         return true;
      default:
         return false;
      }
   }

   /// A fully portable constexpr number character check                      
   constexpr bool IsNumerical(char c) noexcept {
      switch (c) {
      case '0': case '1': case '2': case '3': case '4': case '5':
      case '6': case '7': case '8': case '9':
         return true;
      default:
         return false;
      }
   }
   
   /// A fully portable constexpr space character check                       
   constexpr bool IsSpace(char c) noexcept {
      return c == ' ';
   }

   /// Verify that a string literal is made of allowed ASCII symbols          
   constexpr bool IsASCII(auto source) {
      for (char c : source) {
         if (IsAlphabetical(c) or IsOperator(c) or IsNumerical(c) or IsSpace(c))
            continue;
         return false;
      }
      return true;
   }

   namespace CT
   {
      /// Check if all T are Literal types                                    
      template<class...T>
      concept FixedString = Inner::CheckSize<T...>() and (T::CTTI_StringLiteral and ...);

      /// Supported character types used by Literal                           
      template<class...T>
      concept FixedChar = Inner::CheckSize<T...>() and ((
              std::same_as<T, char>
           or std::same_as<T, wchar_t>
           or std::same_as<T, char8_t>
           or std::same_as<T, char16_t>
           or std::same_as<T, char32_t>
         ) and ...);
   }

   using Token = ::std::string_view;


   ///                                                                        
   /// String literal                                                         
   /// You can use it as a template parameter                                 
   /// Should be introduced in C++26 as std::fixed_string, supposedly         
   ///                                                                        
   /// Since literals are unique types, they can't be used in ?: statements,  
   /// so I've allowed string literals of the form `? "\0\0\0" : "alt"` to    
   /// be consistent - left literal has a Literal array size of 3, but size() 
   /// of 0                                                                   
   ///                                                                        
   template<class T, size_t N, class TRAITS = ::std::char_traits<T>>
   struct Literal {
      static constexpr bool CTTI_StringLiteral = true;
      static constexpr size_t ArraySize = N;

      using storage_type = ::std::array<T, N + 1>;
      storage_type _data {0};

      using traits_type = TRAITS;
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
      using view_type = ::std::basic_string_view<value_type, traits_type>;

      static constexpr size_t npos = view_type::npos;

      constexpr Literal() noexcept = default;

      constexpr Literal(const value_type(&array)[N + 1]) noexcept {
         ::std::copy(::std::begin(array), ::std::end(array), _data.begin());
      }

      constexpr Literal& operator = (const value_type(&array)[N + 1]) noexcept {
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
         // This is a slow implementation, but Literals are mostly used 
         // at compile-time, so it shouldn't be an issue                
         auto ptr = _data.data();
         const auto ptrEnd = _data.data() + N;
         while(ptr != ptrEnd and *ptr)
            ++ptr;
         return ptr - _data.data();
      }
      
      constexpr bool empty() const noexcept {
         return not N or not _data[0];
      }
      
      constexpr explicit operator bool () const noexcept {
         return N and _data[0];
      }

      ///                                                                     
      /// Access                                                              
      ///                                                                     
      constexpr decltype(auto) operator [] (this auto&& self, size_t n) has_assumptions {
         #if LANGULUS_SAFE()
            if not consteval {
               if (n >= self.size()) throw ::std::range_error(HERE());
            }
         #endif
         return self._data[n];
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
      using Resized = Literal<value_type, M, traits_type>;

   protected:
      template<class, size_t, class>
      friend struct Literal;

      template<size_t pos, size_t count, size_t size>
      consteval static size_t clamp() {
         if constexpr (pos >= size)
            return 0;
         return count < size - pos ? count : size - pos;
      }

      constexpr view_type sv() const { return *this; }

   public:

      /// Implicit cast to a string view                                      
      constexpr operator view_type() const noexcept {
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
      constexpr size_t find(view_type sv, size_t pos = 0) const noexcept {
         return sv().find(sv, pos);
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
      constexpr size_t rfind(view_type sv, size_t pos = npos) const noexcept {
         return sv().rfind(sv, pos);
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
      constexpr size_t find_first_of(view_type sv, size_t pos = 0) const noexcept {
         return sv().find_first_of(sv, pos);
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
      constexpr size_t find_last_of(view_type sv, size_t pos = npos) const noexcept {
         return sv().find_last_of(sv, pos);
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
      constexpr size_t find_first_not_of(view_type sv, size_t pos = 0) const noexcept {
         return sv().find_first_not_of(sv, pos);
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
      constexpr size_t find_last_not_of(view_type sv, size_t pos = npos) const noexcept {
         return sv().find_last_not_of(sv, pos);
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
         return not empty() and traits_type::eq(front(), c);
      }
      constexpr bool starts_with(const value_type* s) const noexcept {
         return starts_with(view_type(s));
      }

      /// Ends with                                                           
      constexpr bool ends_with(view_type sv) const noexcept {
         return size() >= sv.size() && compare(size() - sv.size(), npos, sv) == 0;
      }
      constexpr bool ends_with(value_type c) const noexcept {
         return !empty() && traits_type::eq(back(), c);
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

   Literal() -> Literal<char, 0>;

   template<class TChar, size_t N>
   Literal(const TChar(&)[N]) -> Literal<TChar, N - 1>;


   /// Swap two literals                                                      
   template<CT::FixedString S>
   void swap(S& lhs, S& rhs) noexcept(noexcept(lhs.swap(rhs))) {
      lhs.swap(rhs);
   }


   ///                                                                        
   /// Literal == Literal                                                     
   constexpr bool operator == (
      const CT::FixedString auto& lhs,
      const CT::FixedString auto& rhs
   ) {
      if (lhs.size() != rhs.size())
         return false;
      
      for (size_t i = 0; i < lhs.size(); ++i) {
         if (lhs[i] != rhs[i])
            return false;
      }
      return true;
   }

   /// Literal == View                                                        
   template<CT::FixedString S>
   constexpr bool operator == (const S& lhs, typename S::view_type rhs) {
      return static_cast<typename S::view_type>(lhs) == rhs;
   }

   /// View == Literal                                                        
   template<CT::FixedString S>
   constexpr bool operator == (typename S::view_type lhs, const S& rhs) {
      return lhs == static_cast<typename S::view_type>(rhs);
   }

   /// Literal == Array                                                       
   template<CT::FixedString S, size_t N>
   constexpr bool operator == (const S& lhs, const typename S::value_type(&rhs)[N]) {
      return lhs == static_cast<typename S::view_type>(rhs);
   }

   /// Array == Literal                                                       
   template<CT::FixedString S, size_t N>
   constexpr bool operator == (const typename S::value_type(&lhs)[N], const S& rhs) {
      return static_cast<typename S::view_type>(lhs) == rhs;
   }


   ///                                                                        
   /// Literal <=> Literal                                                    
   constexpr auto operator <=> (
      const CT::FixedString auto& lhs,
      const CT::FixedString auto& rhs
   ) {
      using lhs_type = std::decay_t<decltype(lhs)>;
      using sv_type = typename lhs_type::view_type;
      return static_cast<sv_type>(lhs) <=> rhs;
   }

   /// Literal <=> View                                                       
   template<CT::FixedString S>
   constexpr auto operator <=> (const S& lhs, const typename S::view_type& rhs) {
      return static_cast<typename S::view_type>(lhs) <=> rhs;
   }
   
   /// View <=> Literal                                                       
   template<CT::FixedString S>
   constexpr auto operator <=> (const typename S::view_type& lhs, const S& rhs) {
      return lhs <=> static_cast<typename S::view_type>(rhs);
   }
   
   /// Literal <=> Array                                                      
   template<CT::FixedString S, size_t N>
   constexpr auto operator <=> (const S& lhs, const typename S::value_type(&rhs)[N]) {
      using sv_type = typename S::view_type;
      return static_cast<sv_type>(lhs) <=> sv_type {rhs};
   }
   
   /// Array <=> Literal                                                      
   template<CT::FixedString S, size_t N>
   constexpr auto operator <=> (const typename S::value_type(&lhs)[N], const S& rhs) {
      using sv_type = typename S::view_type;
      return sv_type {lhs} <=> static_cast<sv_type>(rhs);
   }
   

   ///                                                                        
   /// Concatenation                                                          
   ///                                                                        
   template<CT::FixedString LHS, CT::FixedString RHS>
   constexpr auto operator + (const LHS& lhs, const RHS& rhs) {
      typename LHS::template Resized<LHS::ArraySize + RHS::ArraySize> result;
      size_t i = 0;
      for (; i < lhs.size(); ++i)
         result[i] = lhs[i];
      for (; i < lhs.size() + rhs.size(); ++i)
         result[i] = rhs[i - lhs.size()];
      return result;
   }

   template<CT::FixedChar C, size_t N>
   constexpr auto operator + (const C(&lhs)[N], const CT::FixedString auto& rhs) {
      Literal lhs2 = lhs;
      return lhs2 + rhs;
   }

   template<CT::FixedChar C, size_t N>
   constexpr auto operator + (const CT::FixedString auto& lhs, const C(&rhs)[N]) {
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

   constexpr auto operator + (CT::FixedChar auto lhs, const CT::FixedString auto& rhs) {
      return Inner::from_char(lhs) + rhs;
   }

   constexpr auto operator + (const CT::FixedString auto& lhs, CT::FixedChar auto rhs) {
      return lhs + Inner::from_char(rhs);
   }

   /// Equivalent to Yes, but also carries a string literal                   
   /*template<Literal TEXT>
   struct YesText {
      static constexpr Literal Constant = TEXT;
      static constexpr bool Enabled = true;
   };*/
   
   template<Literal TEXT>
   using YesText = YesValue<TEXT>;
   
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
